program main
  call part1()
  call part2()

contains
  function get_score(c) result(score)
    character, intent(in) :: c
    integer :: score
    if (c > 'Z') then
      score = ichar(c) - ichar('a') + 1
    else
      score = ichar(c) - ichar('A') + 27
    end if
  end function get_score

  subroutine part1()
    character(80) :: line
    integer :: ios, length, half_len, i, score=0

    open(unit=10, file='input.txt', form='formatted', status='old')
    do
      read(10, '(A)', iostat=ios) line
      if (ios /= 0) exit
      length = len_trim(line)
      half_len = length/2
      i = scan(line(1:half_len), line(half_len+1:length))
      score = score + get_score(line(i:i))
    end do
    close(10)
    print *, 'part 1 ', score
  end subroutine part1

  subroutine part2()
    integer :: ios,score=0,i
    character(len=80) :: line1, line2, line3
    character(len=:), allocatable :: tmp1, tmp2

    open(unit=10, file='input.txt', form='formatted', status='old')
    do
      read(10, '(A)', iostat=ios) line1
      read(10, '(A)', iostat=ios) line2
      read(10, '(A)', iostat=ios) line3
      if (ios /= 0) exit
      tmp1 = intersect(line1, line2)
      tmp2 = intersect(line2, line3)
      i = scan(tmp1, tmp2)
      score = score + get_score(tmp1(i:i))
    end do
    close(10)
    print *, "part 2 ", score
  end subroutine part2

  function intersect(a, b) result(s)
    integer :: i, j
    character(len=*) :: a, b
    character(len=:), allocatable :: s
    character :: c

    do i = 1, len_trim(a)
      c = a(i:i)
      if (char_exists(b,c)) then
        s = s // c
      end if
    end do
  end function intersect

  function char_exists(s, c) result(b)
    character(len=*), intent(in) :: s
    character, intent(in) :: c
    logical :: b
    b = (index(s, c)) .ne. 0
  end function char_exists
end program main
