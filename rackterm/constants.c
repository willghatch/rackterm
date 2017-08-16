#include <stdio.h>
#include <sys/ioctl.h>

int main() {
    printf("TIOCSWINSZ #x%x\n", TIOCSWINSZ);
    printf("TIOCGWINSZ #x%x\n", TIOCGWINSZ);
    printf("TIOCSCTTY #x%x\n", TIOCSCTTY);
    printf("TIOCNOTTY #x%x\n", TIOCNOTTY);
    return 0;
}
