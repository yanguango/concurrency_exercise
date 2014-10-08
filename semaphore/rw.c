#include <stdio.h>
#include <stdlib.h>

int main(int argc, char* argv[]) {
    FILE* in;
    FILE* out;
    char* buffer = NULL;
    size_t len = 0;
    ssize_t curr_line_len;

    if (argc != 3) {
        fprintf(stderr, "USAGE:./transfer1 <INPUT> <OUTPUT>\n");
        exit(0);
    }

    in = fopen(argv[1], "r");
    out = fopen(argv[2], "w");
    if (in == NULL || out == NULL) {
        exit(EXIT_FAILURE); 
    }

    while ((curr_line_len = getline(&buffer, &len, in)) != -1) {
        printf("%s", buffer);
        fwrite(buffer, 1, curr_line_len, out);
    }
    
    free(buffer);
    fclose(in);
    fclose(out);
    
    return 0;
}
