program main
  implicit none
  character :: direction
  integer :: ios, steps, x, y
  complex :: head = (0, 0), tail = (0, 0), diff = (0, 0)
  complex, allocatable, dimension(:) :: visited

  visited = [cmplx(0, 0)]
  open(10, file='input.txt', form='formatted', status='old')
  do
    read(10, '(A,1x,i2)', iostat=ios) direction, steps
    if (ios /= 0) exit
    x = 0; y = 0
    if (direction == 'R') x = 1
    if (direction == 'L') x = -1
    if (direction == 'U') y = 1
    if (direction == 'D') y = -1

    do steps=steps,1,-1
      head = head + cmplx(x, y)
      diff = head - tail
      if (abs(real(diff)) > 1) then
        tail = tail + cmplx(x, 0)
        if (imag(diff) > 0) tail = tail + cmplx(0, 1)
        if (imag(diff) < 0) tail = tail - cmplx(0, 1)
      else if (abs(imag(diff)) > 1) then
        tail = tail + cmplx(0, y)
        if (real(diff) > 0) tail = tail + cmplx(1, 0)
        if (real(diff) < 0) tail = tail - cmplx(1, 0)
      end if
      if (.not. any(visited == tail)) visited = [visited, tail]
    end do
  end do
  print *, "Part 1 ", size(visited)
  close(10)
end program main
