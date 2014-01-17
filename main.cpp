#include <errno.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <unistd.h>

#include <iostream>
#include <string>

int main(int argc, char* argv[])
{
	auto str = std::string();

	for (int i = 1; i != argc; ++i)
	{
		str += argv[i];
		str += " ";
	}

	switch (fork())
	{
		case -1: // Error
			std::cerr << "Error on fork()\n";
			break;
		case 0: // Child process
			//close(1); // Close stdout
			//close(2); // Close stderr
			//open("/dev/null", O_WRONLY); // Redirect stdout to /dev/null
			//open("/dev/null", O_WRONLY); // Redirect stderr to /dev/null
			if (-1 == execl("/bin/bash", "bash", "-c", str.c_str(), nullptr))
			{
				std::cerr << "Error on exec()\n";
			}
			break;
		default: // Parent process
		{
			int status;
			if (wait(&status) <= 0)
			{
				std::cerr << "Error on wait()\n";
			}
			       
			if (WIFEXITED(status))
			{
				if (WEXITSTATUS(status) == 0)
				{
					return 0;
				}
			}
		} break;
	}
}
