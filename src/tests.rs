#[cfg(test)]

mod tests {

use crate::image::*;

#[test]
fn image_iter_win() {
    let mut image = Image::new_grey(5, 5);

    image.l.set(0, 0, 1.0);
    image.l.set(4, 0, 2.0);
    image.l.set(4, 4, 3.0);
    image.l.set(0, 4, 4.0);

    let mut res_arr = [[0_f32; 5]; 5];
    for (x, y, v) in image.l.iter_rect_crd(0, 0, 4, 4) {
        res_arr[x as usize][y as usize] = v;
    }

    assert!(res_arr[0][0] == 1.0);
    assert!(res_arr[4][0] == 2.0);
    assert!(res_arr[4][4] == 3.0);
    assert!(res_arr[0][4] == 4.0);
}

#[test]
fn area_iter() {
    let width = 7;
    let height = 7;
    let x_cnt = 5;
    let y_cnt = 5;
    for (i, j, area) in RectAreaIterator::new(width, x_cnt, height, y_cnt) {
        if i == x_cnt-1 { assert!(area.x2 == width-1); }
        if j == y_cnt-1 { assert!(area.y2 == height-1); }
        if i == 0 { assert!(area.x1 == 0); }
        if j == 0 { assert!(area.y1 == 0); }
    }
}

} // mod tests