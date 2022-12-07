program main
  implicit none
  integer :: tmp = 0, ans = 0
  open(10, file='input.txt', form='formatted', status='old')
  call solve(10, tmp, ans)
  print *, "Part 1: ", ans
  close(10)
end program main

recursive subroutine solve(fd, bytes, answer)
  integer, intent(in) :: fd
  integer, intent(inout) :: bytes, answer
  integer :: children_bytes, tmp
  integer :: ios
  character(len=80) :: line

  do
    read(fd, '(A)', iostat=ios) line

    if (ios /= 0 .or. line(1:7) == '$ cd ..') then
      if (bytes <= 100000) then
        answer = answer + bytes
      end if
      exit
    end if

    if (line(1:4) == '$ cd' .and. line(1:6) /= '$ cd /') then
      children_bytes = 0
      call solve(fd, children_bytes, answer)
      bytes = bytes + children_bytes
    end if

    if (line(1:1) == '$' .or. line(1:3) == 'dir') cycle
    read(line, *) tmp
    bytes = bytes + tmp
  end do
end subroutine solve
