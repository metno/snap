#! /usr/bin/perl
use strict;
use warnings;

use FindBin;
use Getopt::Long qw(GetOptions);
use Pod::Usage qw(pod2usage);
use Data::Dumper qw(Dumper);
use HTTP::Daemon;
use HTTP::Status;
use CGI qw();

use vars qw(%Args $Daemon %Plants);

chdir($FindBin::Bin) or die "Cannot chdir $FindBin::Bin: $!";

umask 0000; # make sure all files are writable by all

GetOptions(\%Args,
       'debug',
       'help')
    or pod2usage(2);

pod2usage(-exitval => 0,
      -verbose => 2) if $Args{help};
readPlants();
startDaemon();

sub readPlants {
	open my $f, "$FindBin::Bin/npps.csv"
		or die "Cannot read $FindBin::Bin/npps.csv: $!";
	scalar <$f>; # skip header
	while (defined (my $line = <$f>)) {
		chomp $line;
		my ($site, $country, $long, $lat, $status) = split (/\|/, $line);
		unless (defined $lat) {
			print STDERR "problem reading line $line\n";
		}
		my $tag = $site;
		$tag =~ s/ /_/g;
		$Plants{$tag} = {site => $site, CC => $country, lon => $long, lat => $lat, status => $status };
	}
}

sub startDaemon {
    my $port = 8081;
    my $host = 'localhost';
	$Daemon = HTTP::Daemon->new(
    	           LocalAddr => $host,
                   LocalPort => $port,
                   ReuseAddr => 1, # make sure to start immediately
	);
    print STDERR "SNAP-runner started at http://$host:$port/snaprunner in $FindBin::Bin\n";
	while (my $c = $Daemon->accept) {
    	while (my $r = $c->get_request) {
        	if ($r->method eq 'GET' and $r->uri->path =~  m^/snaprunner^) {
        		my $action = $r->uri->path;
        		$action =~ s:/snaprunner/([^\?])/?\?*:$1:;
                $c->send_response(handleCGI($action, new CGI($r->url->query)));
            } else {
            	# POST 
            	#my $req = $conn->get_request;
        		#my $cgi = new CGI($r->content); 
                $c->send_error(RC_FORBIDDEN)
            }
        }
        $c->close;
       	undef($c);
	}
}

sub endDaemon {
	$Daemon->close();
	exit(0);
}

# the screens

sub runModel {
	my ($cgi) = @_;
	our %Plants;
	my $errors = "";
	my $params = $cgi->Vars;
	
	my $startTime;
	if ($params->{startTime} =~ /(\d{4})-(\d{2})-(\d{2})\s+(\d{1,2})/) {
		$startTime = "$1 $2 $3 $4";
	} else {
		$errors .= "Cannot interprete startTime: $params->{startTime}<br>\n";
	}
	if (! $params->{runTime} =~ /\d+/) {
		$errors .= "Cannot interprete runTime: $params->{runTime}<br>\n";
	}
	if (! $params->{releaseTime} =~ /\d+/) {
		$errors .= "Cannot interprete releaseTime: $params->{releaseTime}<br>\n";
	}
	my ($lat, $lon, $npp);
	$lat = $params->{latitude};
	$lon = $params->{longitude};
	if (exists $params->{npp} and $params->{npp} and exists $Plants{$params->{npp}}) {
		$npp = $Plants{$params->{npp}}{site};
		$lat = $Plants{$params->{npp}}{lat};
		$lon = $Plants{$params->{npp}}{lon};
	}
	if (!($lat =~ /-?\d+\.?\d*/ and (abs($lat) <= 90))) {
		$errors .= "Cannot interprete latitude: $lat<br>\n";
	}
	if (! ($lon =~ /-?\d+\.?\d*/ and (abs($lon) <= 90))) {
		$errors .= "Cannot interprete longitude: $lon<br>\n";
	}
	if ($errors) {
		return "<html><head><title>SNAP-Runner</title></head><body><h1>SNAP-Runner</h1><h2>Errors</h2>".$errors."</body></html>";
	}
	
	open (my $snapf, "$FindBin::Bin/snap.input.tmpl") 
		or die "Cannot read $FindBin::Bin/snap.input.tmpl: $!\n";
	my $input;
	{
		local $/ = undef;
		$input = <$snapf>;
	}
	close $snapf;
	$input =~ s/^SET_RELEASE.POS.*$//m;
	$input =~ s/^TIME.START.*$//m;
	$input =~ s/^TIME.RUN.*$//m;
    $input =~ s/^RELEASE.HOUR.*$/RELEASE.HOUR= 0, $params->{releaseTime}/m;
    # $input =~ s/^FIELD.OUTPUT=.*$//m;
    #$input =~ s/^LOG.FILE=.*$//m;
    $input = "SET_RELEASE.POS= P=   $lat,   $lon\nTIME.START= $startTime\nTIME.RUN = $params->{runTime}h\n" . $input;

    my $inputFile = "snap.input";
    open (my $oh, ">$inputFile") or die "Cannot write file $inputFile: $!\n";
    print $oh $input;
    close $oh;

    my $releaseScenario = $input;
    $releaseScenario =~ s/^(?!RELEASE).*$//gm;
    $releaseScenario =~ s/^(?=RELEASE\.POS).*$//gm;
    $releaseScenario =~ s:^((?=RELEASE).*)$:<pre>$1</pre>:gm;
	
	# fix diana-setup
	{
	    open (my $diTmpl, "$FindBin::Bin/diana.setup.tmpl") 
			or die "Cannot read $FindBin::Bin/diana.setup.tmpl: $!\n";
		local $/ = undef;
		my $d = <$diTmpl>;
		$d =~ s/%PWD%/$FindBin::Bin/g;
		open ($oh, ">diana.setup")
			or die "Cannot write diana.setup: $!\n";
		print $oh $d;
		close $oh;
		close $diTmpl;
	}
    my $command = "./bsnap $inputFile";
    print STDERR $command, "\n";
	system($command);
	$command = "fimex --input.file=snap.felt --input.config=felt2nc_snap.xml --output.file=snap.nc --output.type=nc4";
    print STDERR $command, "\n";
	system($command);
    unlink("snap.felt");
    chmod(0666, "snap.felt_level_names");
	system("diana.bin -s diana.setup&");
	
	
	return "<html><head><title>SNAP-Runner</title></head><body><h1>SNAP-Runner</h1>SNAP run successfull for: <p>Time: $params->{startTime} Length $params->{runTime}h<p>Place $npp<br>Lat: $lat<br>Lon: $lon<br><p>Release Scenario:<br>$releaseScenario<p> Start diana with <pre>diana.bin -s $FindBin::Bin/diana.setup</pre><a href=\"default\">Start new run.</a></body></html>";
}

sub debug {
	my ($cgi) = @_;
    return "<html><body><h1>Debug snaprunner</h1></body></html>".
    	CGI->escapeHTML(scalar Dumper(\%ENV, $cgi));
}

sub startScreen {
	my ($cgi) = @_;
	our %Plants;
	open (my $f, "$FindBin::Bin/startScreen.html")
		or die "Cannot read $FindBin::Bin/startScreen.html\n";
	my $html;
	{
		local $/ = undef;
		$html = <$f>;
	}
	my $options = "";
	foreach my $tag (sort keys %Plants) {
		$options .= "<option value=\"$tag\">".$Plants{$tag}{site}."</options>\n";
	}
	$html =~ s/%NPP_OPTIONS%/$options/;
    my @date = gmtime(time);
    my $currentTime = sprintf ("%d-%02d-%02d %02d:00", $date[5]+1900,$date[4]+1,$date[3],$date[2]);
    $html =~ s/%CURRENTTIME%/$currentTime/;
	return $html;
}


# the screen-handler
sub handleCGI {
	my ($action, $cgi) = @_;
	my %actions = (run => \&runModel,
				   default => \&startScreen,
				   debug => \&debug,
				   close => \&endDaemon,
				   quit => \&endDaemon,
	);
	my $func = $actions{$action} || $actions{default};
	print STDERR "$action\n";
	my $rspStr = $func->($cgi);
	return HTTP::Response->new(
            200, 'OK',
            HTTP::Headers->new(Content_Type => "text/html"),
            $rspStr);
}

END {
	print "closing daemon\n";
	$Daemon->close();
}


__END__

=head1 NAME

snapRunner.pl - TBD

=head1 SYNOPSIS

=head1 DESCRIPTION

=head2 GENERAL SYNTAX

=over 4

=item

=back

=head1 EXAMPLE

=head1 AUTHOR

Heiko Klein, E<lt>Heiko.Klein@met.noE<gt>

=head1 SEE ALSO

L<perl>.

=cut
