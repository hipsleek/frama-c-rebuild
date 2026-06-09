_Bool get_bool(void);
_Bool pass_bool(_Bool b);

_Bool b_flag1, b_flag2;

void main(){

    b_flag1 = pass_bool( !(_Bool)get_bool() );
    b_flag2 = pass_bool( (_Bool)get_bool() );
}
