module test_axis_collection
    use, intrinsic :: iso_fortran_env
    use :: testdrive, only:new_unittest, unittest_type
    use :: test_axis_unitTests_initialValues
    use :: test_axis_unitTests_getLength
    use :: test_axis_unitTests_typeBoundProcedure
    implicit none
    private
    public :: collect

contains
    subroutine collect(test_suite)
        implicit none
        type(unittest_type), allocatable, intent(out) :: test_suite(:)
            !! collection of tests

        test_suite = [ &
                     new_unittest("initial value of coordinates should be 0.", &
                                  initial_value_should_be_0) &
                     , new_unittest("get_length should return max coord val - min coord val.", &
                                    get_length_should_returns_max_coord_val_minus_min_coord_val) &
                     , new_unittest("Temporarily behavior check of type-bound procedure.", &
                                    test_type_bound_procedures) &
                     ]
    end subroutine collect
end module test_axis_collection
