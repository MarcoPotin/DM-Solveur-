#paramètre
DEBUG=-fcheck=all -ffpe-trap=invalid,zero
Compile=gfortran $(DEBUG)


#édition des liens
prog.exe : solveur.o mod_solveur.o 
	$(Compile) -o prog.exe solveur.o mod_solveur.o
	@echo compilation terminée

#Compilation
mod_solveur.o : mod_solveur.f90
	$(Compile) -c mod_solveur.f90
solveur.o : solveur.f90 mod_solveur.o
	$(Compile) -c solveur.f90


#destruction des fichiers
clean :
	rm -f *.o *.mod *.exe

dat :
	rm -f *.dat
