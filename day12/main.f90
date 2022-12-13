program main
  implicit none
  character(len=1024) :: line
  integer :: ios, x, y, cols, rows = 0
  integer*1 :: c
  integer*1, allocatable, dimension(:,:) :: grid
  logical, allocatable, dimension(:,:) :: mask, visited
  integer, allocatable, dimension(:,:) :: costs
  integer :: start(2), dest(2)

  open(10, file='input.txt', status='old')
  read(10, *, iostat=ios) line
  cols = len_trim(line)
  do while(ios == 0)
    read(10, *, iostat=ios)
    rows = rows + 1
  end do
  rewind(10)
  allocate(grid(rows,cols), mask(rows,cols), visited(rows, cols), costs(rows,cols))
  do y=1,rows
    read(10, '(*(A))') grid(y,:)
  end do
  close(10)

  start = findloc(grid, 83)
  dest = findloc(grid, 69)
  grid(start(1),start(2)) = 97 ! set start to a
  grid(dest(1),dest(2)) = 122 ! set end to z

  costs = 0
  visited = .false.
  mask = .false.
  mask(start(1),start(2)) = .true.

  do
    start = minloc(costs, mask=mask)
    x = start(1); y = start(2)
    mask(x,y) = .false.

    if (visited(x, y)) cycle
    if (all(start == dest)) exit

    visited(x,y) = .true.
    if (can_walk(x+1,y,grid(x,y),.false.)) call push_mask(x+1,y, costs(x,y))
    if (can_walk(x-1,y,grid(x,y),.false.)) call push_mask(x-1,y, costs(x,y))
    if (can_walk(x,y+1,grid(x,y),.false.)) call push_mask(x,y+1, costs(x,y))
    if (can_walk(x,y-1,grid(x,y),.false.)) call push_mask(x,y-1, costs(x,y))
  end do

  print *, "Part 1: ", costs(dest(1), dest(2))

  costs = 0
  visited = .false.
  mask = .false.
  mask(dest(1),dest(2)) = .true.
  do
    start = minloc(costs, mask=mask)
    x = start(1); y = start(2)
    mask(x,y) = .false.

    if (visited(x, y)) cycle
    if (grid(x,y) == 97) exit

    visited(x,y) = .true.
    if (can_walk(x+1,y,grid(x,y),.true.)) call push_mask(x+1,y, costs(x,y))
    if (can_walk(x-1,y,grid(x,y),.true.)) call push_mask(x-1,y, costs(x,y))
    if (can_walk(x,y+1,grid(x,y),.true.)) call push_mask(x,y+1, costs(x,y))
    if (can_walk(x,y-1,grid(x,y),.true.)) call push_mask(x,y-1, costs(x,y))
  end do

  print *, "Part 2: ", costs(x, y)
contains
  function can_walk(r, c, v, reverse) result(res)
    integer :: r, c
    integer*1 :: v
    logical :: res, reverse
    res = .false.
    if (r > 0 .and. r <= rows &
      .and. c > 0 .and. c <= cols) then
      if (.not. reverse .and. grid(r, c) - v <= 1) res = .true.
      if (reverse .and. v- grid(r, c) <= 1) res = .true.
    end if
  end function can_walk

  subroutine push_mask(a, b, c)
    integer, intent(in) :: a, b, c
    mask(a,b) = .true.
    costs(a,b) = c + 1
  end subroutine push_mask
end program main
