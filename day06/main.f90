program main
  implicit none
  integer :: ios, i
  character(len=4) :: str1
  character(len=14) :: str2
  character :: c
  logical :: has

  open(10, file='input.txt', form='formatted', status='old')

  ! part 1
  read(10, '(A)', advance='no', iostat=ios) str1
  i = 0
  do
    has = check_dups(str1)
    if (.not. has) exit
    if (ios /= 0) exit
    read(10, '(A)', advance='no', iostat=ios) c
    str1 = str1(2:4) // c
    i = i +1
  end do
  print *, "Part 1 ", (i + 4)

  rewind(10)
  ! part 2
  read(10, '(A)', advance='no', iostat=ios) str2
  i = 0
  do
    has = check_dups(str2)
    if (.not. has) exit
    if (ios /= 0) exit
    read(10, '(A)', advance='no', iostat=ios) c
    str2 = str2(2:14) // c
    i = i +1
  end do
  print *, "Part 2 ", (i + 14)

  close (10)
contains
  function check_dups(str) result(res)
    character(len=*) :: str
    logical :: res
    logical :: seen(26)
    integer :: i, j

    res = .false.
    seen(1:26) = .false.

    do i=1,len(str)
      j = ichar(str(i:i)) - 96
      if (seen(j)) then
        res = .true.
        exit
      else
        seen(j) = .true.
      end if
    end do
  end function check_dups
end program main
