#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <string.h>
#include <sys/resource.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

#include <algorithm>
#include <cctype>
#include <iostream>
#include <map>
#include <ostream>
#include <string>
#include <tuple>
#include <type_traits>
#include <vector>

struct process_info
{
	pid_t id;
	timeval start_timeval;
};

static_assert(std::is_pod<process_info>::value,
	"Type 'process_info' need to be a POD type to send it via a pipe.");

struct bg_task
{
	int task_id;
	process_info watcher_proc;
	process_info exec_proc;
};
static_assert(std::is_pod<bg_task>::value,
	"Type 'bg_task' need to be a POD type to send it via a pipe.");


std::ostream& operator<<(std::ostream& out, const bg_task& t)
{
	out << "[" << t.task_id << "] " << t.exec_proc.id;
	return out;
}

auto bg_tasks = std::map<int, bg_task>();

int wait_cmd(process_info process)
{
	int status;
	if (-1 == waitpid(process.id, &status, 0))
	{
		std::cerr << "Error on wait(): " << strerror(errno) << "\n";
	}

	rusage ts;
	timeval end;
	timeval wallclock;

	gettimeofday(&end, nullptr);
	timersub(&end, &process.start_timeval, &wallclock);

	if (-1 != getrusage(RUSAGE_CHILDREN, &ts))
	{
		char tmbuf[64];
		timeval tv;
		timeradd(&ts.ru_utime, &ts.ru_stime, &tv);
		std::cout << '\n';
		strftime(tmbuf, 64, "%s", localtime(&tv.tv_sec));
		printf("CPU time: %s.%06ld seconds\n", tmbuf, tv.tv_usec);
		strftime(tmbuf, sizeof tmbuf, "%s", localtime(&wallclock.tv_sec));
		printf("Wall-clock time: %s.%06ld seconds\n", tmbuf, wallclock.tv_usec);
		printf("Voluntary context switches: %ld\n", ts.ru_nvcsw);
		printf("Involuntary context switches: %ld\n", ts.ru_nivcsw);
		printf("Page reclaims (soft page faults): %ld\n", ts.ru_minflt);
		printf("Page faults (hard page faults): %ld\n", ts.ru_majflt);
		std::cout << '\n';
	}

	if (WIFEXITED(status))
	{
		return WEXITSTATUS(status);
	}
	return -1;
}

process_info launch_cmd(std::vector<std::string> args)
{
	pid_t child_pid = fork();
	timeval t;
	gettimeofday(&t, nullptr);
	switch (child_pid)
	{
		case -1: // Error
			std::cerr << "Error on fork(): " << strerror(errno) << "\n";
			return process_info{child_pid, t};

		case 0: // Child process
		{
			const char *file = args[0].c_str();
			char * * argv = new char *[args.size()+1];
			std::vector<std::string>::size_type i;

			for (i = 0; i < args.size(); ++i) {
				argv[i] = const_cast<char*>(args[i].c_str());
			}

			argv[i] = nullptr;

			if (-1 == execvp(file, argv))
			{
				std::cerr << "Error on exec(): " << strerror(errno) << "\n";
				for (auto v : args)
				{
					std::cout << "'" << v << "'\n";
				}
				return process_info{-1, t};
			}
			return process_info{-1, t};
		} break;
		default: // Parent process
		{
			return process_info{child_pid, t};
		}
	}
}

bg_task launch_background_cmd(int task_id, std::vector<std::string> args)
{
	timeval t;
	gettimeofday(&t, nullptr);

	int pipefd[2];
	if (0 == pipe(pipefd))
	{
		pid_t watcher_pid = fork();
		switch (watcher_pid)
		{
			case -1: // Error
			{
				std::cerr << "Error on fork(): " << strerror(errno) << "\n";
				return bg_task{-2, process_info{-1, t}, process_info{}};
			} break;

			case 0: // Child process
			{
				close(pipefd[0]); // Close read end

				auto info = launch_cmd(args);
				write(pipefd[1], &info, sizeof(process_info));
				wait_cmd(info);
				// bg_tasks.erase(info.id);

				std::exit(EXIT_SUCCESS);
			} break;

			default: // Parent process
			{
				close(pipefd[1]); // Close write end

				process_info info;
				read(pipefd[0], &info, sizeof(process_info));

				return bg_task{task_id, process_info{watcher_pid, t}, info};
			} break;
		}
	}
	else
	{
		std::cerr << "Error on pipe(): " << strerror(errno) << "\n";
		return bg_task{-1, process_info{-1, t}, process_info{}};
	}
}

void cd(std::string dir)
{
	if (chdir(dir.c_str()) == -1)
	{
		std::cout << strerror(errno) << "\n";
	}
}

void check_processes()
{
	/* Check if processes need to be removed from bg_tasks */
	for (std::map<int, bg_task>::const_iterator it=bg_tasks.begin(); it!=bg_tasks.end(); ++it) {
		int status;
		int ret = waitpid(it->second.watcher_proc.id, &status, WNOHANG);
		if (ret == -1 || (WEXITSTATUS(status) == 0 && ret != 0))
		{
			bg_tasks.erase(it->second.exec_proc.id);
		}
	}
}

int main(int /* argc */, char* /* argv */[])
{
	timeval start;
	gettimeofday(&start, nullptr);

	int next_task_id = 0;

	bool quit = false;
	while (not quit)
	{
		std::string line;
		std::cout << get_current_dir_name() << "> ";
		std::cout.flush();
		if (not std::getline(std::cin, line) || line == "exit")
		{
			std::cout << std::endl;
			quit = true;
		}
		else
		{
			auto is_whitespace = [](char c){
				return std::isspace(c) != 0;
			};
			auto begin = std::find_if_not(line.begin(), line.end(), is_whitespace);
			auto end = line.end();
			auto result = begin;
			std::vector<std::string> args;
			while (begin != end)
			{
				result = std::find(begin, end, ' ');
				args.emplace_back(begin, result);
				begin = std::find_if_not(result, end, is_whitespace);
			}

			if (not args.empty())
			{
				if (args.back() == "&")
				{
					args.pop_back();
					auto task = launch_background_cmd(next_task_id++, args);
					std::cout << task << std::endl;
					bg_tasks[task.exec_proc.id] = task;
				}
				else if (args[0] == "cd")
				{
					cd(args[1]);
				}
				else if (args[0] == "aptaches")
				{
					check_processes();
					for (std::map<int, bg_task>::const_iterator it=bg_tasks.begin(); it!=bg_tasks.end(); ++it) {
						std::cout << it->second << std::endl;
					}
				}
				else
				{
					wait_cmd(launch_cmd(args));
				}
			}
		}
	}
	/* Kill everything in bg_tasks */
	check_processes();
	for (std::map<int, bg_task>::const_iterator it=bg_tasks.begin(); it!=bg_tasks.end(); ++it) {
		std::cout << "\nKilling " << it->first;
		kill(it->second.exec_proc.id, SIGTERM);
	}
}
