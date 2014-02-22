# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

if [ -f /home/simon/github/SimonEnv/Bash/SimonBashGeneral ]; then
	source /home/simon/github/SimonEnv/Bash/SimonBashGeneral
fi

#if [ -f /home/simon/github/SimonEnv/Bash/SimonWorkSettings ]; then
#	source /home/simon/github/SimonEnv/Bash/SimonWorkSettings
#fi

if [ -f /home/simon/github/SimonEnv/Bash/SimonHomeSettings ]; then
	source /home/simon/github/SimonEnv/Bash/SimonHomeSettings
fi