#include <stdio.h>

// Obfuscated quick sort

void print_a(int*, int, int);

void algo2(int* a, int s, int e){
	if( s >= e ){
		return;
	}
	int p = *(a + s);
	int b = s;
	int t = e+1;
	while(1){
		do {
			b++;
		} while (b <= e && *(a+b)<p);
		do {
			t--;
		} while (*(a+t)>p);
		if(b>t){
			break;
		}
		int c = *(a+b);
		*(a+b) = *(a+t);
		*(a+t) = c;
	}
	int d = *(a+s);
	*(a+s) = *(a+t);
	*(a+t) = d;
	algo2(a, s, t-1);
	algo2(a, t+1, e);
}

void print_a(int* a, int s, int e){
	int i;
	for(i=s; i<=e; i++){
		printf("%i",*(a+i));
		if( i != e){
			printf(",");
		}
	}
	printf("\n");
}

int main(){
	int a[3] = { 3,1,2 };
	printf("Initial a\n");
	print_a(a, 0, 2);
	algo2( a, 0, 2);
	printf("Sorted a\n");
	print_a(a, 0, 2);

	int a2[20] = { 4,3,6,2,3,2,8,5,1,5,6,7,8,2,1,2,5,6,3,4 };		
	print_a(a2,0,19);
	algo2( a2, 0, 19);
	print_a(a2,0,19);

}


