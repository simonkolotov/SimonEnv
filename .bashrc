# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

export SimonEnvBashPath=/home/simon/github/SimonEnv/Bash/

if [ -f $SimonEnvBashPath"SimonBashGeneral" ]; then
	source /home/simon/github/SimonEnv/Bash/SimonBashGeneral
fi

#if [ -f $SimonEnvBashPath"SimonWorkSettings" ]; then
#	source /home/simon/github/SimonEnv/Bash/SimonWorkSettings
#fi

if [ -f $SimonEnvBashPath"SimonHomeSettings" ]; then
	source /home/simon/github/SimonEnv/Bash/SimonHomeSettings
fi