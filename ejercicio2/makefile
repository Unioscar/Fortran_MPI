ej2: 	ejercicio2.f90
	mpif90 constantes.f90 ejercicio2.f90 -o ej2.exe
memory:	
	mpif90 -fbounds-check -o free constantes.f90 ejercicio2.f90 
memory_run:	
	./free
run:	
	mpirun -np 4 ej2.exe
clear:	ej2
	rm -r ej2.exe free constantes.mod
