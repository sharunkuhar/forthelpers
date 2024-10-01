program main
    implicit none
    integer, parameter :: cgreal = selected_real_kind(p=14,r=30)
    integer :: istart, iend, idump, ntime, n, npart, npcount, pstackcount, itemp, npstack
    character*80 :: in_fname, out_fname
    real(kind=cgreal) :: ftemp
    real(kind=cgreal),dimension(:),allocatable :: x, y, z, vx, vy, vz
    ! real :: ftemp
    ! real,dimension(:),allocatable :: x, y, z, vx, vy, vz

    istart = 108001
    iend = 128000
    idump = 100

    pstackcount = 1000

    npart = 10000

    allocate(x(npart), y(npart), z(npart), vx(npart), vy(npart), vz(npart))
    
    npstack = istart/pstackcount
    
    write(in_fname,'("path",i5.5,".dat")') npstack
    write(*,*) 'Opening File: ',trim(in_fname)
    open(101,file=trim(in_fname),form='unformatted',access='stream')

    npcount = 0

    do ntime = istart,iend
        
        ! Read the path file:

        if(npcount == pstackcount) then
            close(101)
            npstack = npstack+1
            npcount = 0
            write(in_fname,'("path",i5.5,".dat")') npstack
            write(*,*) 'Opening File: ',trim(in_fname)

            open(101,file=trim(in_fname),form='unformatted',access='stream')
        endif
        do n = 1, npart
            read(101)x(n), y(n), z(n), vx(n), vy(n), vz(n), itemp, ftemp
        enddo
        npcount = npcount + 1

        
        if(mod(ntime,idump)==0) then
            ! Write the csv file:
            write(out_fname,'("path",i7.7,".csv")') ntime
            write(*,*) 'Writing File: ', trim(out_fname)
            open(102,file=trim(out_fname))
            write(102,*) 'x, y, z, vx, vy, vz'

            do n = 1,Npart
                write(102,*) x(n), ',', y(n), ',', z(n), ',', vx(n), ',', vy(n), ',', vz(n)
            enddo

            close(102)
        endif
    
    enddo


end program main
