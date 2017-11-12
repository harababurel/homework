use polynomial::Polynomial;

pub fn seq_product(a: &Polynomial, b: &Polynomial) -> Polynomial {
    let mut product = Polynomial::new();

    for i in 0..a.degree() {
        for j in 0..b.degree() {
            *product.get_mut(i + j) += a.get(i) * b.get(j);
        }
    }

    product
}
