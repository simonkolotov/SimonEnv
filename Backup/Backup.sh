# copy this file to /home/simon/scripts/Backup.sh

TimeStamp=$(date +%Y-%m-%d)
BackupLog=/home/simon/Backup/Backup-$TimeStamp.log
MotionBackupLog=/home/simon/Backup/MotionBackup-$TimeStamp.log

if [ -f /home/simon/github/SimonEnv/Bash/SimonWorkBackup ]; then
	  source /home/simon/github/SimonEnv/Bash/SimonWorkBackup &> $BackupLog
    source /home/simon/github/SimonEnv/Bash/SimonWorkMotionBackup &> $MotionBackupLog
else
    echo Backup Command Missing &> $BackupLog
fi