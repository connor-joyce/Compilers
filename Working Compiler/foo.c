#include <stdio.h>
#include <stdint.h>

int main(){
	uint8_t x = 1; 
	uint8_t y = 0;
       uint8_t z = x&&y; 	
	printf("%d", z);
       return 0; 	
}
