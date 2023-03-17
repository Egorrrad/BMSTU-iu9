#include <stdio.h>
#include <string.h>
#include <stdlib.h>

int max(int a, int b){
    if(a>b){
        return a;
    }
    return b;
}
void csort(char *src, char *dest){
    char **strings;
    strings=calloc(1000,sizeof(char*));
    char *str;
    str=calloc(10000, sizeof(char));
    str[0]=' ';
    strcat(str, src);
    int len=strlen(str);
    int j=0, k;
    int i, maxk=0;
    int count=0;
    for (i=0; i<1000; i++){
        strings[i]=calloc(100, sizeof(char));
    }
    i=1;
    while (i<len){
        if (str[i-1]==' ' && str[i]!=' '){
            j=0;
            k=count;
            while (str[i]!=' ' && i<len && str[i]!='\0' && str[i]!='\n' ){
                strings[k][j]=str[i];
                j+=1;
                i+=1;
            }
            strings[k][j] = '\0';
            count+=1;
            maxk=max(maxk, strlen(strings[k]));
        }
        else{
            i+=1;
        }
    }

    int l1, l2;
    char *p;
    int *mas;
    maxk+=1;
    mas=malloc(sizeof(int)*maxk);
    for (i=0; i<maxk; i++){
        mas[i]=0;
    }
    for (i=0; i<maxk; i++){
        for(k=0; k<count; k++){
            l1=strlen(strings[k]);
            mas[l1]+=1;
        }
    }
    int c=0;
    for (i=0; i<maxk; i++){
        for(k=0; k<count; k++){
            l1=strlen(strings[k]);
            if(i==l1){
                mas[i]-=1;
                if(mas[i]==0){
                    break;
                }
                if(c==0){
                    strcpy(dest, strings[k]);
                }
                else{
                    strcat(dest, " ");
                    strcat(dest, strings[k]);
                }
                c++;
            }
        }
    }

    free(str);
    for (i=0; i<1000; i++){
        free(strings[i]);
    }
    free(strings);
    free(mas);
}

int main(){
    char *stroka;
    stroka=calloc(1000, sizeof(char));
    fgets(stroka, 1000, stdin);
    int len=strlen(stroka);
    stroka[len-1]='\0';
    char *new;
    new=calloc(1005, sizeof(char));
    csort(stroka, new);
    printf("%s\n", new);
    free(new);
    free(stroka);
    return 0;
}


extern void csort(char *src, char *dest);


#define MEM(p) must_be_not_null((p), #p, __FILE__, __LINE__)

static void *must_be_not_null(void *p, const char *expr,
		const char *file, int line) {
	if (p) return p;

	fprintf(stderr, "%s:%d: '%s' returns NULL\n", file, line, expr);
	abort();
}


enum { MAX_WORD_LEN = 12, NVOWELS = 6, NCONSONANTS = 20 };

static const char VOWELS[] = "aeiouy";
static const char CONSONANTS[] = "bcdfghjklmnpqrstvwxz";


static int randint(int limit)
{
	return (int) (rand() / ((double) RAND_MAX + 1) * limit);
}

#define RANDLETTER(type) (type[randint(N ## type)])


struct Word {
	char letters[MAX_WORD_LEN];
	size_t len;
	size_t spaces_after;
	size_t order;
};


static void init_word(struct Word *word, size_t order)
{
	size_t len = MAX_WORD_LEN / 2 + randint(MAX_WORD_LEN / 2);
	size_t i = 0;

	while (i < len - 2) switch (randint(4)) {
	case 0:
		word->letters[i++] = RANDLETTER(VOWELS);
		break;

	case 1:
		word->letters[i++] = RANDLETTER(CONSONANTS);
		break;

	case 2:
		word->letters[i++] = RANDLETTER(CONSONANTS);
		word->letters[i++] = RANDLETTER(VOWELS);
		break;

	case 3:
		word->letters[i++] = RANDLETTER(VOWELS);
		word->letters[i++] = RANDLETTER(CONSONANTS);
		break;
	}
	word->letters[i] = '\0';

	word->len = i;
	word->spaces_after = 1 + randint(5);
	word->order = order;
}


static int word_compare(const void *vleft, const void *vright)
{
	const struct Word *left = vleft;
	const struct Word *right = vright;

	return left->len != right->len
		? (int) left->len - (int) right->len
		: (int) left->order - (int) right->order;

}


static int random_test(size_t nwords)
{
	// init
	struct Word words[nwords];
	size_t source_len = 0, dest_len = 0;

	for (size_t i = 0; i < nwords; ++i) {
		init_word(&words[i], i);
		source_len += words[i].len + words[i].spaces_after;
		dest_len += words[i].len + 1;
	}

	source_len -= words[nwords - 1].spaces_after;
	dest_len -= 1;

	char *source = MEM(malloc(source_len + 1));
	char *dest = MEM(malloc(dest_len + 1));
	char expected[dest_len + 1];

	// init destination buffer with garbage
	for (size_t i = 0; i < dest_len + 1; ++i) dest[i] = 1 + randint(255);

	char *p = source;
	for (size_t i = 0; i < nwords; ++i) {
		memcpy(p, words[i].letters, words[i].len);
		p += words[i].len;
		if (i != nwords - 1) {
			for (size_t j = 0; j < words[i].spaces_after; ++j) *p++ = ' ';
		}
	}
	*p = '\0';

	printf("Source string:\n%s\n", source);

	qsort(words, nwords, sizeof(words[0]), word_compare);

	p = expected;
	for (size_t i = 0; i < nwords; ++i) {
		memcpy(p, words[i].letters, words[i].len);
		p += words[i].len;
		*p++ = ' ';
	}
	p[-1] = '\0';

	printf("Expected result:\n%s\n", expected);
	fflush(stdout);

	char source_copy[source_len + 1];
	strcpy(source_copy, source);

	// call
	csort(source, dest);


	// check
	printf("Result after sorting:\n%s\n", dest);
	int mismatched_result = strcmp(dest, expected) != 0;
	if (mismatched_result) printf("TEST FAILED! Strings are not equal!\n\n");


	int source_touched = memcmp(source, source_copy, source_len + 1);
	if (source_touched) printf("TEST FAILED! Source string is changed!\n\n");

	int failed = mismatched_result || source_touched;
	if (! failed) printf("Test passed!\n\n");


	free(dest);
	free(source);
	return failed;
}


int main()
{
	int failed = 0;

	failed += random_test(1);
	failed += random_test(2);
	failed += random_test(3);
	failed += random_test(10);
	failed += random_test(30);
	failed += random_test(10 + randint(10));
	failed += random_test(200);

	return failed == 0 ? EXIT_SUCCESS : EXIT_FAILURE;
}

