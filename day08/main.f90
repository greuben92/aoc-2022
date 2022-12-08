program main
  implicit none
  integer, parameter :: size_m = 99
  integer :: heights(size_m, size_m), scores(size_m, size_m)
  integer :: ios, i, j, visible

  open(10, file='input.txt', form='formatted', status='old')
  i = 1
  do
    read(10, '(*(i1))', iostat=ios) heights(:, i)
    if (ios /= 0) exit
    i = i + 1
  end do

  call part1()
  call part2()

  close(10)
contains
  subroutine part1()
    visible = size_m*4 - 4

    do i=2,size_m-1
      do j=2,size_m-1
        if (&
          all(heights(j,i) > heights(j, :i-1))&
          .or. all(heights(j,i) > heights(j, i+1:))&
          .or. all(heights(j,i) > heights(:j-1, i))&
          .or. all(heights(j,i) > heights(j+1:, i))&
        ) then
          visible = visible + 1
        end if
      end do
    end do
    print *, 'Part 1', visible
  end subroutine part1

  subroutine part2()
    integer :: l,r,u,d
    scores = 0
    do i=1,size_m-1
      do j=1,size_m-1
        l = findloc(heights(j, i) > heights(j, : i - 1), .false., back=.true., dim=1)
        if (l == 0) l = 1
        r = findloc(heights(j, i) > heights(j, i + 1 : ), .false., dim=1) + i
        if (r == i) r = size_m
        u = findloc(heights(j, i) > heights(: j - 1, i), .false., back=.true., dim=1)
        if (u == 0) u = 1
        d = findloc(heights(j, i) > heights(j + 1 :, i), .false., dim=1) + j
        if (d == j) d = size_m
        scores(j, i) = (i - l) * (r - i) * (j - u) * (d - j)
      end do
    end do
    print *, 'Part 2 ', maxval(scores)
  end subroutine part2
end program main
