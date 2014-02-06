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
#include <string>
#include <tuple>
#include <vector>

using process_info = std::tuple<pid_t, timeval>;
using bg_task = std::tuple<int, process_info>;

process_info launch_cmd(std::vector<std::string> args)
{
	pid_t child_pid = fork();
	timeval t;
	gettimeofday(&t, nullptr);
	switch (child_pid)
	{
		case -1: // Error
			std::cerr << "Error on fork(): " << strerror(errno) << "\n";
			return process_info(child_pid, t);

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
				return process_info(-1, t);
			}
			return process_info(-1, t);
		} break;
		default: // Parent process
		{
			return process_info(child_pid, t);
		}
	}
}

int wait_cmd(process_info process)
{
	int status;
	if (-1 == waitpid(std::get<0>(process), &status, 0))
	{
		std::cerr << "Error on wait(): " << strerror(errno) << "\n";
	}

	rusage ts;
	timeval end;
	timeval wallclock;

	gettimeofday(&end, nullptr);
	timersub(&end, &std::get<1>(process), &wallclock);

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

void cd(std::string dir)
{
	if (chdir(dir.c_str()) == -1)
	{
		std::cout << strerror(errno) << "\n";
	}
}

int main(int /* argc */, char* /* argv */[])
{
	timeval start;
	gettimeofday(&start, nullptr);

	auto bg_tasks = std::vector<bg_task>();
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
					bg_tasks.push_back(bg_task(next_task_id++, launch_cmd(args)));
				}
				else if (args[0] == "cd")
				{
					cd(args[1]);
				}
				else if (args[0] == "aptaches")
				{
					for (auto x : bg_tasks)
					{
						std::cout << "[" << std::get<0>(x) << "] " << std::get<0>(std::get<1>(x)) << "\n";
					}
				}
				else
				{
					wait_cmd(launch_cmd(args));
				}
			}
		}
	}

}

