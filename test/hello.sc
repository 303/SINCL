
/* stdio.hはインクルード出来ないので
   とりあえずextern宣言してください */
extern int puts(const char*);

int
main(void)
{
    puts("Hello World");
    return 0;
}
