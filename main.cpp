#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <sys/resource.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/wait.h>
#include <unistd.h>

#include <iostream>
#include <string>
#include <tuple>
#include <vector>

<<<<<<< HEAD
int main(int /* argc */, char* argv[])
{
	timeval start;
	gettimeofday(&start, nullptr);

	switch (fork())
=======
using process_info = std::tuple<pid_t, timeval>;

process_info launch_cmd(std::vector<std::string> args)
{
	pid_t child_pid = fork();
	timeval t;
	gettimeofday(&t, nullptr);
	switch (child_pid)
>>>>>>> 9f950e0... cmd abstraction
	{
		case -1: // Error
			std::cerr << "Error on fork()\n";
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
				std::cerr << "Error on exec()\n";
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

int main(int /* argc */, char* /* argv */[])
{
	timeval start;
	gettimeofday(&start, nullptr);

	std::vector<std::string> args = {"ls", "-lahF"};
	launch_cmd(args);
}


/*
int status;
			if (wait(&status) <= 0)
			{
				std::cerr << "Error on wait()\n";
			}

			rusage ts;
			timeval end;
			timeval wallclock;

			gettimeofday(&end, nullptr);
			timersub(&end, &start, &wallclock);

			if (-1 != getrusage(RUSAGE_CHILDREN, &ts))
			{
				char tmbuf[64];
				timeval tv;
				timeradd(&ts.ru_utime, &ts.ru_stime, &tv);
				strftime(tmbuf, 64, "%s", localtime(&tv.tv_sec));
				printf("CPU time: %s.%06ld seconds\n", tmbuf, tv.tv_usec);
				strftime(tmbuf, sizeof tmbuf, "%s", localtime(&wallclock.tv_sec));
				printf("Wall-clock time: %s.%06ld seconds\n", tmbuf, wallclock.tv_usec);
				printf("Voluntary context switches: %ld\n", ts.ru_nvcsw);
				printf("Involuntary context switches: %ld\n", ts.ru_nivcsw);
				printf("Page reclaims (soft page faults): %ld\n", ts.ru_minflt);
				printf("Page faults (hard page faults): %ld\n", ts.ru_majflt);
			}

			if (WIFEXITED(status))
			{
				return WEXITSTATUS(status);
			}
		} break;
*/
