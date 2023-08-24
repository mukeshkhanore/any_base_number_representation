!This is code written by Mukesh Khnore to Represent given integer n to base+1 representation.
!Working criteria base >= n
program number_rep
    implicit none
    integer,allocatable::array(:)
    integer::i,j,base,n,ni,r_sum,count1,k
    write(*,*) "Please provide the base and number"
    read(*,*)base , n
    write(*,*) "Given n = ", n, "base= ", base 
    base=base+1
    count1=0    
    allocate(array(base))    
    open(unit=5,file="output.dat")
    i=0
    array=0
    do 
        i=i+1
        ni=i
        do j=1,base
            array(j)=mod(ni,base)
            ni=int(ni/base)
            if(ni==0)exit
        enddo    
        r_sum=sum(array)
        if(r_Sum==n.and.array(base)==0)then
            count1=count1+1
            write(*,*)count1
            write(5,*)(array(k),k=1,base-1)
        endif
        if(array(base-1)==n)exit
    enddo
    close(5)
endprogram number_rep
