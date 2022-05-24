window.addEventListener('load', () => {
    function getIndex() {
        const index = window.BNF_INDEX;
        if (index == null) {
            throw Error('BNF_INDEX is missing.');
        }
        return index;
    }

    const spans = document.querySelectorAll('.language-bnf .non-terminal');
    for (const span of spans) {
        const index = getIndex();
        const nonTerminal = span.textContent;
        if (Object.hasOwn(index, nonTerminal)) {
            const a = document.createElement('a');
            a.href = index[nonTerminal];
            a.classList = span.classList;
            a.innerHTML = span.innerHTML;
            span.replaceWith(a);
        }
    }
});
