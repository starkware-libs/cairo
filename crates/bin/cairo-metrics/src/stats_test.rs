use super::*;

#[test]
fn test_mean_simple() {
    let stats = compute_stats(&[1, 2, 3], 3, 1).unwrap();
    assert_eq!(stats.mean_ns, 2);
}

#[test]
fn test_median_odd() {
    let stats = compute_stats(&[3, 1, 2], 3, 1).unwrap();
    assert_eq!(stats.median_ns, 2);
}

#[test]
fn test_median_even() {
    // [1, 2, 3, 4] -> median = (2 + 3) / 2 = 2.5, rounds to 3.
    let stats = compute_stats(&[1, 2, 3, 4], 4, 1).unwrap();
    assert_eq!(stats.median_ns, 3);
}

#[test]
fn test_min_max() {
    let stats = compute_stats(&[5, 1, 3], 3, 1).unwrap();
    assert_eq!(stats.min_ns, 1);
    assert_eq!(stats.max_ns, 5);
}

#[test]
fn test_stddev() {
    // [1, 2, 3] -> mean=2, variance=((1-2)^2 + (2-2)^2 + (3-2)^2) / 2 = 1, stddev=1.
    let stats = compute_stats(&[1, 2, 3], 3, 1).unwrap();
    assert_eq!(stats.stddev_ns, 1);
}

#[test]
fn test_stddev_single_value() {
    let stats = compute_stats(&[5], 1, 0).unwrap();
    assert_eq!(stats.stddev_ns, 0);
}

#[test]
fn test_mad_simple() {
    // [1, 2, 3, 4, 5] -> median=3, deviations=[2, 1, 0, 1, 2], sorted=[0, 1, 1, 2, 2], MAD=1.
    let stats = compute_stats(&[1, 2, 3, 4, 5], 5, 1).unwrap();
    assert_eq!(stats.mad_ns, 1);
}

#[test]
fn test_mad_too_few_samples() {
    let stats = compute_stats(&[1, 2], 2, 0).unwrap();
    assert_eq!(stats.mad_ns, 0);
}

#[test]
fn test_empty_times() {
    let stats = compute_stats(&[], 0, 0).unwrap();
    assert_eq!(stats.mean_ns, 0);
    assert_eq!(stats.median_ns, 0);
    assert_eq!(stats.min_ns, 0);
    assert_eq!(stats.max_ns, 0);
}

#[test]
fn test_compute_mad_standalone() {
    assert_eq!(compute_mad(&[1, 2, 3, 4, 5], 3), 1);
}
