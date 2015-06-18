# copy this file to /home/simon/scripts/Backup.sh

TimeStamp=$(date +%Y-%m-%d)
BackupLog=/home/simon/Incoming/Backup/Backup-$TimeStamp.log

if [ -f /home/simon/github/SimonEnv/Bash/SimonWorkBackup ]; then
	  source /home/simon/github/SimonEnv/Bash/SimonWorkBackup &> $BackupLog
else
    echo Backup Command Missing &> $BackupLog
fi