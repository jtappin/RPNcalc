#!/usr/bin/perl

# Convert the readable(ish) source of the RPN calc help into someting that
# can be pasted as a Fortran/C interoperable string constant.

my $helpfile='rpncalc.txt';
my $writefile='tmp.txt';

open(IN, $helpfile) || die "Failed to open $helpfile: $!\n";
open(OUT, '>'.$writefile) || die "Failed to open $writefile: $!\n";

while (<IN>) {
    s/\'/\'\'/;
    chop;
    $_ = "& '".$_;
    if (eof(IN)) {
	$_ = $_."'//cnull &\n";
    } else {
	$_ = $_."'//c_new_line// &\n";
    }

    print OUT $_;
}
close IN;
close OUT;
