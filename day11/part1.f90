program main
  implicit none
  integer :: ios, tmp, i, j, k, opval, throw
  character(len=100) :: line
  type monkey
    integer, allocatable, dimension(:) :: items
    character :: op
    logical :: opSelf
    integer :: opval, divisible, throwTrue, throwFalse
  end type monkey
  type(monkey), allocatable :: m
  type(monkey), allocatable, dimension(:) :: monkeys
  integer, allocatable, dimension(:) :: maxIn
  integer :: prime = 1

  open(10, file='input.txt', form='formatted', status='old')
  do
    allocate(m)
    read(10, *)
    read(10, '(A16)', advance='no') line
    do
      read(10, '(1X, I3)', advance='no', iostat=ios) tmp
      if (ios /= 0) exit
      if (allocated(m%items)) then
        m%items = [m%items, tmp]
      else
        m%items = [tmp]
      end if
    end do
    read(10, '(A)') line
    m%op = line(24:24)
    if (line(26:28) == 'old') then
      m%opSelf = .true.
      m%opval = 0
    else
      m%opSelf = .false.
      read(line(26:), *) m%opval
    end if
    read(10, '(T22,I5)') m%divisible
    read(10, '(T30,I5)') m%throwTrue
    read(10, '(T30,I5)') m%throwFalse
    read(10, *, iostat=ios) ! empty line
    prime = prime * m%divisible

    if (allocated(monkeys)) then
      monkeys = [monkeys, m]
    else
      monkeys = [m]
    end if
    deallocate(m)

    if (ios /= 0) exit
  end do

  allocate(maxIn(size(monkeys)))
  maxIn = 0
  do i=1,20
    do j=1,size(monkeys)
      do k=1,size(monkeys(j)%items)
        tmp = monkeys(j)%items(1)
        opval = monkeys(j)%opval
        if (monkeys(j)%opSelf) opval = tmp
        if (monkeys(j)%op == '*') tmp = tmp * opval
        if (monkeys(j)%op == '+') tmp = tmp + opval
        tmp = tmp / 3
        if (mod(tmp, monkeys(j)%divisible) == 0) then
          throw = 1 + monkeys(j)%throwTrue
        else
          throw = 1 + monkeys(j)%throwFalse
        end if
        monkeys(throw)%items = [monkeys(throw)%items, mod(tmp, prime)]
        monkeys(j)%items = monkeys(j)%items(2:)
        maxIn(j) = maxIn(j) + 1
      end do
    end do
  end do

  i = maxval(maxIn)
  j = maxval(pack(maxIn, maxIn /= i))
  print *, "Part 1 ", i * j

  close(10)
end program main
