# Crontab Scripts

Runs scripts in ~/.config/cron/

## NOTE

They MUST:<br>

- be readable
- be executable 

Every script generates a log in `/tmp/` on run.

## Rules:<br>

- reboot
- every day
- every hour
- every week
- every fortnight
- every month
- every year
- every day (working weeek)

The file `crontab` should be copied to the buffer created by `crontab -e`.
