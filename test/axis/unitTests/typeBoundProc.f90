module test_axis_unitTests_typeBoundProcedure
    use, intrinsic :: iso_fortran_env
    use :: testdrive, only:error_type, check, to_string
    use :: testdrive_util, only:occurred
    use :: fassert
    use :: axis
    implicit none
    private
    public :: test_type_bound_procedures

contains
    !>Temporarily behavior check of type-bound procedure. need to refactor
    subroutine test_type_bound_procedures(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler
        logical :: stat
        character(:), allocatable :: msg

        type(axis_type) :: ax, ax2

        ax = new_axis([-1d0, 1d0])

        call expect_equal(ax%get_coord_values("min"), -1d0, &
                          "min coord val should be -1d0", &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_equal(ax%get_coord_values("max"), 1d0, &
                          "max coord val should be 1d0", &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_equal(ax%get_coord_values(), [-1d0, 1d0], &
                          "coord val should be [-1d0, 1d0]", &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_equal(ax%get_length(), 2d0, &
                          "get length should return 2d0", &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        ax = [-2d0, 2d0]
        call expect_equal(ax%get_coord_values(), [-2d0, 2d0], &
                          "coord val should be [-2d0, 2d0]", &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        ax2 = ax
        call expect_equal(ax2%get_length(), 4d0, &
                          "get_length() should return 4d0", &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine test_type_bound_procedures
end module test_axis_unitTests_typeBoundProcedure
