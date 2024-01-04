# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

# When using my own user
export SimonEnvBashPath=/home/simon/github/SimonEnv/Bash/
# Otherwise make sure to export this variable over ssh:
# In Putty or MobaXTerm:
# Under "Advanced SSH settings/Execute Command":
# env -u SimonEnvBashPath SimonEnvBashPath=/home/simon/github/SimonEnv/Bash/ bash

if [ -f $SimonEnvBashPath"SimonBashGeneral" ]; then
	source /home/simon/github/SimonEnv/Bash/SimonBashGeneral
fi

#if [ -f $SimonEnvBashPath"SimonWorkSettings" ]; then
#	source /home/simon/github/SimonEnv/Bash/SimonWorkSettings
#fi

if [ -f $SimonEnvBashPath"SimonHomeSettings" ]; then
	source /home/simon/github/SimonEnv/Bash/SimonHomeSettings
fi