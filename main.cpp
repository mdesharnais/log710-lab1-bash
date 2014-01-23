#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <sys/resource.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/wait.h>
#include <unistd.h>

#include <iostream>

int main(int /* argc */, char* argv[])
{
	timeval start;
	gettimeofday(&start, nullptr);

	switch (fork())
	{
		case -1: // Error
			std::cerr << "Error on fork()\n";
			return -1;
		case 0: // Child process
			if (-1 == execvp(argv[1], &argv[1]))
			{
				std::cerr << "Error on exec()\n";
				return -1;
			}
			std::cout << "test";
			break;
		default: // Parent process
		{
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
	}
}
