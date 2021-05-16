function init() {
    document.getElementById('search').addEventListener("submit", function(event) {
        event.preventDefault();
        answer(event);
    }, false);
}

function answer(_event) {
    question = document.getElementById('question').value;
    questionEncoded = encodeURI(question);
    fetchOptions = {
        method: 'GET',
        mode: 'no-cors',
        headers: {
            'Accept': 'application/json'
        }
    };
    fetch('/answer/' + questionEncoded, fetchOptions)
    .then(response => response.json())
    .then(json => render_answer(json));
}

function render_answer(json) {
    answerElem = document.getElementById('answer');
    removeAllChildren(answerElem);
    if (json[0] == "shrug") {
        answerText = document.createTextNode("¯\\_(ツ)_/¯")
    } else {
        answerText = document.createTextNode(json[0].short);
    }
    answerElem.appendChild(answerText);
}

function removeAllChildren(node) {
    while (node.firstChild) {
        node.removeChild(node.lastChild)
    }
}

window.onload = (_event) => {
    init();
};
