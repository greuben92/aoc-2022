program main
  implicit none
  integer :: ios, tmp = 0, total = 0, carry = 0, ans
  character(len=80) :: line

  open(10, file='input.txt', form='formatted', status='old')
  do
    read(10, '(A)', iostat=ios) line
    if (ios /= 0) exit
    if (line(1:1) == '$' .or. line(1:3) == 'dir') cycle
    read(line, *) tmp
    total = total + tmp
  end do
  rewind(10)
  ans = total
  total = total - (70000000-30000000)
  call solve(total, tmp, carry, ans)
  print *, "Part 2 ", ans
  close(10)
end program main

recursive subroutine solve(needed, bytes, carry, answer)
  integer, intent(in) :: needed
  integer, intent(inout) :: bytes, carry, answer
  integer :: children_bytes, tmp
  integer :: ios
  character(len=80) :: line

  do
    read(10, '(A)', iostat=ios) line

    if (ios /= 0 .or. line(1:7) == '$ cd ..') then
      tmp = carry + bytes
      if (tmp >= needed .and. answer >= tmp) then
        answer = tmp
      endif
      exit
    end if

    if (line(1:4) == '$ cd' .and. line(1:6) /= '$ cd /') then
      children_bytes = 0
      call solve(needed, children_bytes, carry, answer)
      bytes = bytes + children_bytes
    end if

    if (line(1:1) == '$' .or. line(1:3) == 'dir') cycle
    read(line, *) tmp
    bytes = bytes + tmp
  end do
end subroutine solve
