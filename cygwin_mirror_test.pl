#!/usr/bin/perl
#
# CygwinMirrorTest.pl
# v1.0 -jwsmythe
#
# https://sourceforge.net/p/cygwinmirrortest
#
# This will find the fastest local(ish) mirrors to you. 
# it evaluates both the ping (latency) and speed (throughput).

# We require the following to already be installed. They should be in the default install.
#
# This script will work under Cygwin or on (almost) any Unix machine.
#
# These are the Cygwin setup locations.
# All -> Web  -> wget
# All -> Base -> sed 

$maxping = '100'; # this is the maximum ping we will accept.  
		  # Anything worse is probably across an ocean,
		  # or on Cogent. :)

# Note: we're not supporting rsync with this test.  If you want it, 
# make your decision based on http/ftp speeds.

use Time::HiRes qw(time);
use Net::Ping;
$phttp = Net::Ping->new("tcp", 2);
$pftp  = Net::Ping->new("tcp", 2);
$phttp->port_number(scalar(getservbyname("http", "tcp")));
$pftp->port_number(scalar(getservbyname("ftp", "tcp")));
$phttp->hires();
$pftp->hires();

if (!-e cygwin_mirror.list){
  print "Collecting mirrors list from http://cygwin.com/mirrors.html\n";
  unlink("mirrors.html");
  system("wget -o /dev/null http://cygwin.com/mirrors.html");
  system("cat mirrors.html | grep -A500 'Site list by region' | grep -B500 'Mirror Administrators' | sed -e s/\\ /\\\\n/g | sed -e s/\\,/\\\\n/g  > tmp.1");
  system("cat tmp.1 | sed -e s/\\</\\\\n/g | grep href | grep -v 'rsync' | cut -f 2 -d \\\" | sort | uniq > cygwin_mirror.list ");
  unlink("tmp.1");
  unlink("mirrors.html");
};

@list = `cat cygwin_mirror.list`;

open (OUT, ">cygwin_mirror_test_result.txt");

foreach $thismirror(@list){
   $thismirror =~ s/\n//g;
   $thismirror =~ s/\ //g; 
   @urlpcs = split("/", $thismirror);
   $thissvc  = $urlpcs[0];
   $thishost = $urlpcs[2];

   # First ping it, so you know if it has an acceptable speed.
  
   if ("$thissvc" eq "http:"){ 
     ($ret, $pingsec, $ip) = $phttp->ping($thishost);
     $pingms = int($pingsec * 1000);
   }elsif("$thissvc" eq "ftp:"){
     ($ret, $pingsec, $ip) = $pftp->ping($thishost);
     $pingms = int($pingsec * 1000);
   }else{ 
     print "!!! Invalid service type $thissvc for $thishost.  Refusing.\n";
   };
   
   # If the latency is ok, test the throughput.

   if ($pingms > $maxping){
      print "--- Skipping $thishost.  Ping $pingms ms > $maxping ms \n";
   }else{  
      print "+++ Testing $thishost.  Ping $pingms acceptable \n";
      unlink("setup.ini");
      $starttime = time();
      $res = `wget -o/dev/null $thismirror/x86_64/setup.ini `;
      $endtime = time();
      unlink("setup.ini");
      $elapsed = $endtime - $starttime;
      print "$elapsed sec $thismirror \n";
      print OUT "$elapsed sec $thismirror \n";
   };
};

print "\n----------------------------------------\n";
print "The results of this test are available in\n";
print "cygwin_mirror_test_result.txt\n";
print "\n";
print "These are your 20 best choices\n";
system("cat cygwin_mirror_test_result.txt | sort -n -k 1 | head -20");
