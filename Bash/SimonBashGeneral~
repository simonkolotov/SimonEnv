# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

# User specific aliases and functions
export HALCONROOT=/usr/local/halcon
export HALCONIMAGES=/mnt/xjetsrv/public/Groups/Software/Installs/HALCON/images 
export PE_HOME=/home/simon/git/SilverJet/
export LD_LIBRARY_PATH=/home/simon/git/SolarJet/Apps/SolarJet/Project/qt/BinLinux:/home/simon/git/SolarJet/Apps/SolarJet/Project/qt/BinLinux:/usr/local/halcon/lib/x64-linux2.4-gcc40/:
export PATH=$PATH:/usr/local/halcon/bin/x64-linux2.4-gcc40
export PYTHONPATH=/usr/local/lib/python2.7/site-packages:/usr/local/lib64/python2.7/site-packages:/home/simon/Experiments/Scripts:/home/simon/Lib

#Shortcuts
export SOLARJET=/home/simon/git/SolarJet/Apps/SolarJet/Project/qt/
export SILVERJET=/home/simon/git/SilverJet/Apps/SolarJet/Project/qt/

export XJETSRV=/mnt/xjetsrv/public/
export SWGROUP=/mnt/xjetsrv/public/Groups/Software/

export AUTOMATICA=/mnt/automatica/d/
export AUTOMATICA_SOLARJET=/mnt/automatica/d/git/SolarJet/Apps/SolarJet/Project/qt/

export ALPHA=/mnt/alpha/
export ALPHA_SOLARJET=/mnt/alpha/git/SolarJet/Apps/SolarJet/Project/qt/

#Expand Env. Vars
shopt -s direxpand # enable
#shopt -u direxpand # disable


# Remove long Prompt
PS1=">"

# Command History
bind '"\e[A": history-search-backward'
bind '"\e[B": history-search-forward'
bind '"\eOA": history-search-backward'
bind '"\eOB": history-search-forward'

# Add development libraries to path (for GIV and for HALCON)
export LD_LIBRARY_PATH=/usr/local/lib:/usr/local/halcon/lib/x64-linux2.4-gcc40:
