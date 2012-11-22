haskell-mailclient
==================

simple haskell mail client written from scratch for Computer Networking homework

Basic setup (for Archlinux)
-------------
* postfix
* courier-imap (courier-mta not recommended because it's too powerful \& complicated)
* haskell-network (for compilation)
* hsemail (for compilation)
* ConfigFile (for compilation of client)
* ParseHelp (for compilation of client)
        
Configuration
----------
* see config.ini for an example

Demo run
------------
    [liuexp@liuexp haskell-mailclient]$ ./client 
    
      Mail client v0.1 in Haskell by Liuexp.
    
    
    client [COMMAND] ... [OPTIONS]
    
    Common flags
      -? --help       Display help message
      -V --version    Print version information
    
    client send [OPTIONS]
    
      -f --from	Sender/From field.
      -t --to	Receiver/To field.
      -s --subject  Subject/Title of your message.
      -m --message  The message body.
    
    
    client retr [OPTIONS]
    
      -n --num	Message ID to be retrieved.
    
    client list [OPTIONS]
    
      -n --num	Message ID to be listed, if not specified, all mails will be listed.
    [liuexp@liuexp haskell-mailclient]$ ./client send -f liuexp -t testmail -s "happy thanksgiving day" -m "long time no see"
    Succeeded!
    [liuexp@liuexp haskell-mailclient]$ ./z.sh 
    [liuexp@liuexp haskell-mailclient]$ ./client list
    +OK POP3 clients that break here, they violate STD53.
    1 575
    2 261
    3 239
    4 239
    5 362
    6 0
    7 0
    8 0
    9 362
    10 370
    11 0
    12 0
    13 0
    14 0
    15 0
    16 0
    17 0
    18 0
    19 0
    20 513
    21 937
    22 537
    23 931
    24 0
    25 0
    26 0
    27 440
    28 425
    29 520
    30 404
    31 390
    32 455
    Succeeded!
    [liuexp@liuexp haskell-mailclient]$ ./client list -n 32
    +OK 32 455
    Succeeded!
    [liuexp@liuexp haskell-mailclient]$ ./client retr -n 32
    +OK 455 octets follow.
    Return-Path: <liuexp@liuexp>
    X-Original-To: testmail
    Delivered-To: testmail@liuexp
    Received: from 127.0.0.1 (localhost.localdomain [127.0.0.1])
    by liuexp (Postfix) with ESMTP id 0F29AC18EF
    for <testmail>; Thu, 22 Nov 2012 17:09:17 +0800 (HKT)
    From: "liuexp" <liuexp@liuexp>
    To: "testmail" <testmail@liuexp>
    Subject: happy thanksgiving day
    Date: Thu, 22 Nov 2012 17:09:17 HKT
    Message-Id: <20121122090917.0F29AC18EF@liuexp>
    long time no see
    Succeeded!

Reference
----------------
https://wiki.archlinux.org/index.php/Courier_Email_Server

http://sherlock.heroku.com/blog/2012/02/03/setting-up-postfix-to-use-gmail-as-an-smtp-relay-host-in-archlinux/

https://wiki.archlinux.org/index.php/Postfix

[Real World Haskell](http://book.realworldhaskell.org/)

Zzzz
-----
TBA

