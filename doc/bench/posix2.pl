use POSIX qw/getcwd/;
use POSIX qw/dup2/;
use POSIX qw/:sys_wait_h/;

print getcwd(), "\n";
