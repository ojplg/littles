#include <stdio.h>

/* The purpose of this program is identical to fromdos, which is not
 * on my system for some god-forsaken reason.  I want to remove all the
 * ^M characters.  Goddammit.
 */

main(int argc, char *argv[])
{

  FILE* inputfile;
  FILE* tempfile;
  char c;

  /* There must be two arguments */
  if( argc != 2 ){
    printf("Wrong number of arguments.\n");
    exit(1);
  }

  if ( ( inputfile = fopen( argv[1] , "r" ) ) == NULL ){
    printf("Cannot open file %s\n", *argv[1]);
    exit(2);
  }

  if ( ( tempfile = fopen( "temp_killems" , "w" ) ) == NULL ) {
    printf("Cannot open temporary file.\n");
    exit(3);
  }

  while ( ( c = getc(inputfile)) != EOF ){
    if( c == '\r') ;
    else 
      putc(c , tempfile);
  }

  fclose(inputfile);
  fclose(tempfile);

  rename( "temp_killems" , argv[1] );
  
  return 0;

}
  
