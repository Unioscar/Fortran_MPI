PROGRAM anillo_mpi
    include 'mpif.h'

    integer myrank, numprocs ,ierror, number,request
    integer :: estado(MPI_STATUS_SIZE)

    call MPI_INIT(ierror)
    call MPI_COMM_SIZE(MPI_COMM_WORLD,numprocs,ierror)
    call MPI_COMM_RANK(MPI_COMM_WORLD,myrank,ierror)

    if (myrank.EQ.0) then

        print *, 'Introduce un entero: '
        read * , number

    end if 

    if (myrank.EQ.0) then 

        call MPI_Send(number,1,MPI_INT,myrank+1,8,MPI_COMM_WORLD,ierror)
        call MPI_Recv(number,1,MPI_INT,numprocs-1,8,MPI_COMM_WORLD,estado,ierror)
        print *, 'Soy',myrank,' y he recibido el número', number

    else if (myrank.EQ.numprocs-1) then

        call MPI_recv(number,1,MPI_INT,myrank-1,8,MPI_COMM_WORLD,estado,ierror)
        print *, 'Soy',myrank,' y he recibido el número',number
        number = number + 1 
        call MPI_Send(number,1,MPI_INT,0,8,MPI_COMM_WORLD,ierror)

    else 

        call MPI_Recv(number,1,MPI_INT,myrank-1,8,MPI_COMM_WORLD,estado,ierror)
        print *, 'Soy',myrank,' y he recibido el número',number
        number = number + 1 
        call MPI_Send(number,1,MPI_INT,myrank+1,8,MPI_COMM_WORLD,ierror)

    end if

    call MPI_FINALIZE(ierror)
END PROGRAM