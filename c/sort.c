#include <stdio.h>

// Quick sort implementation

void print_array(int*, int, int);

void swap(int* array, int a, int b){
	int tmp = *(array+a);
	*(array+a) = *(array+b);
	*(array+b) = tmp;
}

void qsort(int* array, int begin, int end){
	if( begin >= end ){
		return;
	}
	int pivot = *(array + begin);
	int bottom = begin;
	int top = end+1;
	while(1){
		do {
			bottom++;
		} while (bottom <= end && *(array+bottom)<pivot);
		do {
			top--;
		} while (*(array+top)>pivot);
		if(bottom>top){
			break;
		}
		swap(array, bottom, top);
	}
	swap(array, begin, top);
	qsort(array, begin, top-1);
	qsort(array, top+1, end);
}

void print_array(int* array, int begin, int end){
	int i;
	for(i=begin; i<=end; i++){
		printf("%i",*(array+i));
		if( i != end){
			printf(",");
		}
	}
	printf("\n");
}

int main(){
	int array[3] = { 3,1,2 };
	printf("Initial array\n");
	print_array(array, 0, 2);
	qsort( array, 0, 2);
	printf("Sorted array\n");
	print_array(array, 0, 2);

	int array2[20] = { 4,3,6,2,3,2,8,5,1,5,6,7,8,2,1,2,5,6,3,4 };		
	print_array(array2,0,19);
	qsort( array2, 0, 19);
	print_array(array2,0,19);

}


