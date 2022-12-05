program main
  implicit none
  character(80) :: line
  integer :: carrying = 0, top(3) = 0, ios, calories = 0

  open(unit=10,file='input.txt',form='formatted',status='old')
  do
    read(10, '(A)', iostat=ios) line
    if (ios /= 0) exit
    if (line == '') then
      carrying = 0
    else
      read(line, *) calories
      carrying = carrying + calories
      if (carrying > top(1)) then
        if (carrying > top(3)) then
          top(1) = top(2)
          top(2) = top(3)
          top(3) = carrying
        else if (carrying > top(2)) then
          top(1) = top(2)
          top(2) = carrying
        else
          top(1) = carrying
        end if
      end if
    end if
  end do
  close(10)
  print *, "part 1: ", top(3)
  print *, "part 2: ", sum(top)
end program main
