module test_axis_unitTests_initialValues
    use, intrinsic :: iso_fortran_env
    use :: testdrive, only:error_type, check, to_string
    use :: testdrive_util, only:occurred
    use :: fassert
    use :: axis
    implicit none
    private
    public :: initial_value_should_be_0

contains
    subroutine initial_value_should_be_0(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(axis_type) :: ax
        real(real64) :: coord_val(2)
        logical :: stat

        coord_val = ax%get_coord_values()

        call expect_equal(coord_val, [0d0, 0d0], "", stat, quiet=.true.)
        call check(error, stat, "")
        if (occurred(error)) return
    end subroutine initial_value_should_be_0
end module test_axis_unitTests_initialValues
