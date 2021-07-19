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
    if (json.answers.length == 0) {
        answerTextNode = document.createTextNode("¯\\_(ツ)_/¯")
    } else {
        answerText = answerToText(json.answers[0]);
        answerTextNode = document.createTextNode(answerText);
    }
    answerElem.appendChild(answerTextNode);
}

function answerToText(answer) {
    txt = ""
    switch(answer.type) {
        case "text":
            txt = answer.answer.text;
            break;
        case "hash":
            txt = answer.answer.hash;
            break;
        case "conversion_result":
            a = answer.answer;
            txt = a.unit_from_number + " " + a.unit_from_name + " = " + a.unit_to_number + " " + a.unit_to_name
            break;
        default:
            txt = "";
    };
    return txt;
}

function removeAllChildren(node) {
    while (node.firstChild) {
        node.removeChild(node.lastChild)
    }
}

window.onload = (_event) => {
    init();
};
