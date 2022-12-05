program main
  integer :: ios
  integer :: v(4), i, j
  character(len=80) :: p1, p2
  integer :: part1=0, part2=0

  open(unit=10, file='input.txt', form='formatted', status='old')
  do
    read(10, *, iostat=ios) p1, p2
    if (ios /= 0) exit
    i = index(p1, '-')
    j = index(p2, '-')
    read(p1(1:i-1), *) v(1)
    read(p1(i+1:len(p1)), *) v(2)
    read(p2(1:j-1), *) v(3)
    read(p2(j+1:len(p2)), *) v(4)
    if ((v(3) >= v(1) .and. v(4) <= v(2)) .or. (v(1) >= v(3) .and. v(2) <= v(4))) then
      part1 = part1 + 1
    end if
    if (v(1) <= v(4) .and. v(2) >= v(3)) then
      part2 = part2 + 1
    end if
  end do
  close(10)
  print *, 'part 1 ', part1
  print *, 'part 2 ', part2
end program main
