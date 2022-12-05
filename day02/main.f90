program main
  implicit none
  integer, parameter :: w=6, l=0, d=3
  integer, parameter :: matrix(3,3) = reshape([ &
    d,w,l, &
    l,d,w, &
    w,l,d], &
    [3,3])
  integer, parameter :: result(3) =[l,d,w]
  character :: a,b
  integer :: op, pp, ps
  integer :: score=0, score2=0, ios

  open(unit=10, file='input.txt', form='formatted', status='old')
  do
    read(10, '(A,1X,A)', iostat=ios) a,b
    if (ios /= 0) exit
    op = (ichar(a) - ichar('A')) + 1
    ps = (ichar(b) - ichar('X')) + 1
    score = score + ps + matrix(ps, op)
    pp = findloc(matrix(:,op),result(ps),1)
    score2 = score2 + pp + matrix(pp, op)
  end do
  close(10)
  print *, 'part 1: ', score
  print *, 'part 2: ', score2
end program main
