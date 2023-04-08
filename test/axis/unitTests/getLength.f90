module test_axis_unitTests_getLength
    use, intrinsic :: iso_fortran_env
    use :: testdrive, only:error_type, check, to_string
    use :: testdrive_util, only:occurred
    use :: fassert
    use :: axis
    implicit none
    private
    public :: get_length_should_returns_max_coord_val_minus_min_coord_val

contains
    subroutine get_length_should_returns_max_coord_val_minus_min_coord_val(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(axis_type) :: ax
        logical :: stat
        character(:), allocatable :: msg

        call setup()

        call expect_equal(ax%get_length(), 5d0, &
                          "get_length(), it should return length between max coord val and min coord val", &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call teardown()
    contains
        subroutine setup()
            call ax%construct(-1d0, 4d0)
        end subroutine setup

        subroutine teardown()
            call ax%construct(0d0, 0d0)
        end subroutine teardown
    end subroutine get_length_should_returns_max_coord_val_minus_min_coord_val
end module test_axis_unitTests_getLength
