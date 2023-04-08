!>座標系の各軸の情報（最小値と最大値）を取り扱う型を提供する．
!>
module axis
    use, intrinsic :: iso_fortran_env
    use, intrinsic :: iso_c_binding
    implicit none
    private
    public :: axis_min_index, axis_max_index
    public :: new_axis

    enum, bind(c)
        enumerator :: axis_min_index = 1
            !! 軸の最小値を参照するための配列添字
        enumerator :: axis_max_index
            !! 軸の最大値を参照するための配列添字
    end enum

    type, public :: axis_type
        real(real64), private :: min = 0d0
            !! 軸の最小値
        real(real64), private :: max = 0d0
            !! 軸の最大値
    contains
        !- constructors
        procedure, public, pass :: construct_by_values
        !* 値に基づいて変数を生成
        procedure, public, pass :: construct_by_array
        !* 配列に基づいて変数を生成
        generic :: construct => construct_by_values, construct_by_array

        !- setters & getters
        procedure, public, pass :: set_coord_values
        !* 軸の最小値と最大値を設定
        procedure, public, pass :: get_coord_values_
        !* 軸の最小値と最大値を取得
        procedure, public, pass :: get_coord_value_component
        !* 軸の最小値もしくは最大値を取得
        procedure, public, pass :: get_coord_values_to
        !* 軸の最小値と最大値を取得
        procedure, public, pass :: get_length
        !* 軸の長さを取得
        generic :: get_coord_values => get_coord_values_, get_coord_value_component

        !- operators
        procedure, public, pass :: assign_array
        !* 軸の最小値と最大値を持った配列を代入
        procedure, public, pass :: assign_axis
        !* `axis`型の変数を代入
        generic :: assignment(=) => assign_array, assign_axis
    end type axis_type

    interface new_axis
        procedure :: new_axis_from_array
        procedure :: new_axis_from_coord_vals
    end interface
contains
    !>引数で指定した軸の最小値，最大値に基づいてaxis型変数を生成する．
    function new_axis_from_coord_vals(min_coord_val, max_coord_val) result(ax)
        implicit none
        real(real64), intent(in) :: min_coord_val
            !! 軸の最小値
        real(real64), intent(in) :: max_coord_val
            !! 軸の最大値

        type(axis_type) :: ax

        call ax%construct(min_coord_val, max_coord_val)
    end function new_axis_from_coord_vals

    !>引数で指定した，軸の最小値，最大値を持つ配列に基づいて
    !>axis型変数を生成する．
    function new_axis_from_array(coord_vals) result(ax)
        implicit none
        real(real64), intent(in) :: coord_vals(2)
            !! 軸の最小値と最大値<br> `[min, max]`の順に格納

        type(axis_type) :: ax

        call ax%construct(coord_vals)
    end function new_axis_from_array

    !>軸の最小値と最大値を持った配列を代入する．
    subroutine assign_array(this, coord_val)
        implicit none
        !&<
        class(axis_type) , intent(inout) :: this
            !! 当該実体仮引数
        real(real64), intent(in)    :: coord_val(2)
            !! 軸の最小値と最大値<br> `[min, max]`の順に格納
        !&>
        this%min = coord_val(axis_min_index)
        this%max = coord_val(axis_max_index)
    end subroutine assign_array

    !>`axis`型の変数の値をコピーする．
    subroutine assign_axis(this, axis)
        implicit none
        !&<
        class(axis_type), intent(inout) :: this
            !! 当該実体仮引数
        type(axis_type), intent(in) :: axis
            !! コピー元の`axis`型変数
        !&>
        this%min = axis%min
        this%max = axis%max
    end subroutine assign_axis

    !------------------------------------------------------------------!
    ! コンストラクタ
    ! 動作は`set_coord_values`と同じであるが，他の型と
    ! インタフェースを統一するために定義

    !>引数で指定した軸の最小値，最大値に基づいてaxis型変数を生成する．
    subroutine construct_by_values(this, min_coord_val, max_coord_val)
        implicit none
        !&<
        class(axis_type), intent(inout) :: this
            !! 当該実体仮引数
        real(real64)    , intent(in) :: min_coord_val
            !! 軸の最小値
        real(real64)    , intent(in) :: max_coord_val
            !! 軸の最大値
        !&>

        call this%set_coord_values(coord_vals=[min_coord_val, max_coord_val])
    end subroutine construct_by_values

    !>引数で指定した，軸の最小値，最大値を持つ配列に基づいて
    !>axis型変数を生成する．
    subroutine construct_by_array(this, coord_vals)
        implicit none
        !&<
        class(axis_type), intent(inout) :: this
            !! 当該実体仮引数
        real(real64)    , intent(in) :: coord_vals(2)
            !! 軸の最大値と最小値
        !&>

        call this%set_coord_values(coord_vals=coord_vals)
    end subroutine construct_by_array

    !------------------------------------------------------------------!
    !>軸の最小値と最大値を設定する．
    subroutine set_coord_values(this, coord_vals)
        implicit none
        !&<
        class(axis_type) , intent(inout) :: this
            !! 当該実体仮引数
        real(real64), intent(in)    :: coord_vals(2)
            !! 軸の最小値と最大値<br> `[min, max]`の順に格納
        !&>

        !|@todo
        !coord_vals(min_coord) > coord_vals(max_coord)
        !の場合のテストの作成
        !@endtodo

        this = coord_vals
    end subroutine set_coord_values

    !>軸の最小値と最大値を取得する．
    function get_coord_values_(this) result(coord_vals)
        implicit none
        class(axis_type), intent(in) :: this
            !! 当該実体仮引数

        real(real64) :: coord_vals(2)
            !! 軸の最小値と最大値<br> `[min, max]`の順に格納

        coord_vals(axis_min_index) = this%min
        coord_vals(axis_max_index) = this%max
    end function get_coord_values_

    !>軸の最小値もしくは最大値を取得する．
    function get_coord_value_component(this, component) result(coord_val)
        implicit none
        !&<
        class(axis_type), intent(in) :: this
            !! 当該実体仮引数
        character(*)    , intent(in) :: component
            !! 取得する成分（最小値もしくは最大値）を
            !! `"min"`もしくは`"max"`で指定<br>
            !! これ以外の場合，値は不定
        !&>

        real(real64) :: coord_val
            !! 軸の最小値もしくは最大値

        select case (component)
        case ("min")
            coord_val = this%min
        case ("max")
            coord_val = this%max
        end select
    end function get_coord_value_component

    !>軸の最小値と最大値を取得する．
    subroutine get_coord_values_to(this, min, max)
        implicit none
        !&<
        class(axis_type), intent(in)    :: this
            !! 当該実体仮引数
        real(real64)    , intent(out)   :: min
            !! 軸の最小値
        real(real64)    , intent(out)   :: max
            !! 軸の最大値
        !&>

        min = this%min
        max = this%max
    end subroutine get_coord_values_to

    !------------------------------------------------------------------!
    !>軸の長さを取得する．
    function get_length(this) result(length)
        implicit none
        class(axis_type), intent(in) :: this
            !! 当該実体仮引数

        real(real64) :: length
            !! 軸の長さ

        length = this%max - this%min
    end function get_length
end module axis
