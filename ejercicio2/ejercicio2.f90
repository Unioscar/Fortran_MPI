program psdotmpi
    !Modulo externo que contiene las variabes constantes del ejercicio 
    USE CONSTANTES
    include 'mpif.h'
   

    integer myrank,i,numprocs, ierror , tam,slice, resto
    real result,sol
    integer :: estado(MPI_STATUS_SIZE)
    !Declaramos los arrays de memoria dinámica 
    real, allocatable :: x(:)
    real, allocatable :: y(:)
    tam = -1
    !result = 0
    call MPI_INIT(ierror)
    call MPI_COMM_RANK(MPI_COMM_WORLD,myrank,ierror)
    call MPI_COMM_SIZE(MPI_COMM_WORLD,numprocs,ierror)

    if(myrank.EQ.0) then 
        !bucle while para comprobar que no se sale de los limites 
        do while (tam > MAXN .OR. tam < 0) 
            print *, 'introduce el tamaño de los vectores: ' 
            read *, tam
        
            if(tam > MAXN .OR. tam < 0) then 
                print *, 'El tamaño no esta dentro de los valores preestablecidos' 
            end if 
        end do
        !Reservamos memoria 
        allocate(x(tam),y(tam))

        do i = 1, tam
            x(i) = 1/real(i+1)
            y(i) = i+1
            !print *, 'Valores de X:    Valores de Y:'
            print *,x(i),y(i)
        enddo
        !Parte que se lleva el proceso padre en caso de no poder dividirse correctamente 
        resto = mod(tam,numprocs) 

    else 
        !Para el resto de procesos su tamaño solo será el de slice
        resto = 0
    end if

    call MPI_Bcast(tam,1,MPI_INT,0,MPI_COMM_WORLD)
    !Tamaño para todas las partes
    slice = tam / numprocs
    if(myrank.EQ.0) then 
        !result = 0
        do i = 1, slice+resto
            result = result + x(i) * y(i)
        enddo
        do i = 1,numprocs - 1 
            !Enviamos desde la posicion siguiente a donde nos encontramos el array x e y 
            call MPI_Send(x(slice+resto+1),slice,MPI_REAL,i,i,MPI_COMM_WORLD,ierror)
            call MPI_Send(y(slice+resto+1),slice,MPI_REAL,i,i,MPI_COMM_WORLD,ierror)
        enddo
    else
        !Reservamos memoria pero en este caso solo de tamaño slice 
        allocate (x(slice),y(slice))
        call MPI_Recv(x,slice,MPI_REAL,0,myrank,MPI_COMM_WORLD,estado,ierror)
        call MPI_Recv(y,slice,MPI_REAL,0,myrank,MPI_COMM_WORLD,estado,ierror)
        !result = 0
        do i = 1, slice
            result = result + x(i) * y(i)
        enddo
    end if 
    !Reducimos los avalores de result de los diferentes procesos en el proceso 0 sumandolos todos 
    call MPI_Reduce(result,sol,1,MPI_REAL,MPI_SUM,0,MPI_COMM_WORLD,ierror)
    
    if(myrank.EQ.0) then 
        print*, 'El resultado es: ', sol 
    end if 

    !Liberamos memoria 
    if(allocated(x)) then 
        deallocate(x)
    end if 
    if(allocated(y)) then 
        deallocate(y)
    end if 
    call MPI_FINALIZE(ierror)
END PROGRAM

