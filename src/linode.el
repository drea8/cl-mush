'(

  ssh 
  (dired "/root@72.14.184.13:/")
  (dired "/root@72.14.184.13:/var/www/html/")

  sftp root@debayou.net
  put /root/mush /root/mush -r
  iptables -I INPUT -p tcp --dport 4444 -j ACCEPT
  iptables -I OUTPUT -p tcp --dport 4444 -j ACCEPT
  iptables -L ;; list open ports
  lsof -i -P ;; list ports used by pid
  
  )
