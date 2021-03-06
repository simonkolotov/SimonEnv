#!/bin/bash

# copy this file to /home/simon/scripts/Backup.sh
# REMEMBER: The file must be executable!!

echo "Starting Schedulled Backup"

TimeStamp=$(date --date="yesterday" +%Y-%m-%d)
BackupLog=/home/simon/Backup/Backup-$TimeStamp.log
MotionBackupLog=/home/simon/Backup/MotionBackup-$TimeStamp.log

echo "Logging to " $BackupLog

if [ -f /home/simon/github/SimonEnv/Backup/SimonWorkBackup ]; then
    source /home/simon/github/SimonEnv/Backup/SimonWorkBackup &> $BackupLog
    source /home/simon/github/SimonEnv/Backup/SimonWorkMotionBackup &> $MotionBackupLog
else
    echo Backup Command Missing &> $BackupLog
fi