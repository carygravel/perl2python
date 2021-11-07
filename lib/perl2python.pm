package perl2python;

use warnings;
use strict;
use PPI;
use Exporter ();
use base qw(Exporter);
our @EXPORT_OK = qw(parse_document);    # symbols to export on request

sub parse_document {
    my ($string_or_file) = @_;
    my $doc = PPI::Document->new($string_or_file);
    return parse_element($doc);
}

sub parse_element {
    my ($element) = @_;
    my $out = q{};
    if ( $element->isa('PPI::Token::Comment') ) {
        if ( $element->{content} =~ /^([#]!.*)perl/xsm ) {
            $out .= $1 . "python3\n";
        }
        else {
            $out .= $element->{content};
        }
        return $out;
    }
    elsif ( $element->isa('PPI::Statement::Include') ) {
        my @imports;
        for my $child ( @{ $element->{children} } ) {
            if ( $child->isa('PPI::Token::Word') ) {
                if ( $child->{content} =~ /(use|warnings|strict)/xsm ) {
                    next;
                }
                else {
                    push @imports, $child->{content};
                }
            }
        }
        if (@imports) {
            $out .= 'import ' . join ' ', @imports . "\n";
        }
        return $out;
    }
    elsif ( $element->isa('PPI::Statement') ) {
        my @statement;
        my $suppress_trailing_nl;
        for my $child ( @{ $element->{children} } ) {
            if ( $child->isa('PPI::Token::Word') ) {
                if ( $child->{content} eq 'print' ) {
                    push @statement, "$child->{content}(";
                }
            }
            elsif ( $child->isa('PPI::Token::Quote') ) {
                push @statement, $child->{content};
                $suppress_trailing_nl =
                  (       $child->isa('PPI::Token::Quote::Double')
                      and $child->{content} =~ /\\n.$/xsm );
            }
        }
        if ( @statement and $statement[0] eq 'print(' ) {
            if ($suppress_trailing_nl) {
                $statement[-1] =
                    substr( $statement[-1], 0, length( $statement[-1] ) - 3 )
                  . substr $statement[-1], 0, 1;
            }
            push @statement, ')';
        }
        if (@statement) {
            $out .= join( ' ', @statement ) . "\n";
        }
        return $out;
    }
    elsif ( $element->isa('PPI::Token::Whitespace') ) {
    }
    if ( exists $element->{children} ) {
        for my $child ( @{ $element->{children} } ) {
            $out .= parse_element($child);
        }
    }
    return $out;
}

1;
