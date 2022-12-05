program main
    implicit none
    integer :: stat
    character(len=:), allocatable :: line, tmp

    integer :: l, i, j, k, cols, rows=-1
    character :: item
    integer :: move, from, to

    type node
        character :: item
        type(node), pointer :: next => null()
    end type node
    type node_ptr
        type(node), pointer :: p => null()
    end type node_ptr
    type(node_ptr), allocatable :: crates(:)
    character(len=:), allocatable :: answer

    open(unit=10, file="input.txt", form="formatted", status="old")
    do
        call get_line(10, tmp, stat)
        if (stat /= 0) exit
        if (tmp == '') exit
        line = tmp
        rows = rows + 1
    end do

    l = len(line)
    do i = 1,l,3
        if (i + 3 > l) exit
        read(line(i:i+3), *) cols
    end do
    rewind(10)

    allocate(crates(cols))

    do i=1,rows
        call get_line(10, line, stat)
        l = len(line)
        k = 1
        do j=1,l,4
            item = line(j+1:j+1)
            if (item /= '') then
                call append_node(crates(k)%p, item)
            end if
            k = k + 1
        end do
    end do

    call get_line(10, line, stat) ! discard number line
    call get_line(10, line, stat) ! discard empty line

    do
        call get_line(10, line, stat)
        if (stat /= 0) exit
        read(line(6:), *) move
        read(line(13:), *) from
        read(line(18:), *) to
        ! call move_nodes(move, crates(from)%p, crates(to)%p) ! part 1
        call move_nodes_group(move, crates(from)%p, crates(to)%p) ! part 2
    end do

    answer = ''
    do i=1,cols
        answer = answer // crates(i)%p%item
    end do
    ! print *, "part 1: ", answer
    print *, "part 2: ", answer

    close(10)
contains
    subroutine get_line(fd, line, stat)
        integer, intent(in) :: fd
        character(len=:), intent(out), allocatable :: line
        integer, intent(out) :: stat

        integer, parameter :: buffer_len = 1024
        character(len=buffer_len) :: buffer
        integer :: size_read

        line = ''
        do
            read (fd, '(A)', iostat=stat, advance='no', size=size_read) buffer
            if (is_iostat_eor(stat)) then
                line = line // buffer(:size_read)
                stat =0
                exit
            else if (stat == 0) then
                line = line // buffer
            else
                exit
            end if
        end do
    end subroutine get_line

    subroutine append_node(head, item)
        type(node), intent(inout), pointer :: head
        character, intent(in) :: item

        type(node), pointer :: current => null()
        type(node), pointer :: new_node => null()

        allocate(new_node)
        new_node%item = item

        current => head
        if (associated(current)) then
            do while (associated(current%next))
                current => current%next
            end do
            current%next => new_node
        else
            head => new_node
        end if
    end subroutine append_node

    subroutine move_nodes(cnt, from, to)
        integer, intent(in) :: cnt
        type(node), pointer, intent(inout) :: from, to
        type(node), pointer :: tmp
        integer :: i

        do i=1,cnt
            tmp => from
            from => from%next
            tmp%next => to
            to => tmp
        end do
    end subroutine move_nodes

    subroutine move_nodes_group(cnt, from, to)
        integer, intent(in) :: cnt
        type(node), pointer, intent(inout) :: from, to
        type(node), pointer :: tmp, next, first
        integer :: i

        first => from
        tmp => from
        do i=1,cnt-1
            tmp => tmp%next
        end do

        from => tmp%next
        tmp%next => to
        to => first
    end subroutine move_nodes_group
end program main
