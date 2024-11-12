pub fn lowpass_filter(data: &mut [f32]) -> Vec<f32> {
    let mut output = vec![0.0; data.len()];
    output[0] = data[0];
    for i in 1..data.len() {
        output[i] = 0.815686 * (data[i] - output[i - 1]);
    }
    output
}

pub fn highpass_filter(data: &mut [f32], coefficient: f32) -> Vec<f32> {
    let mut output = vec![0.0; data.len()];
    output[0] = data[0];
    for i in 1..data.len() {
        output[i] = coefficient * output[i-1] + data[i] - data[i-1];
    }
    output
}