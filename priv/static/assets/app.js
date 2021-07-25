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
        answerNode = document.createTextNode("¯\\_(ツ)_/¯")
    } else {
        answerNode = answerToNode(json.answers[0]);
    }
    answerElem.appendChild(answerNode);
}

function answerToNode(answer) {
    var node;
    switch(answer.type) {
        case "text":
            node = document.createTextNode(answer.answer.text);
            break;
        case "hash":
            node = document.createTextNode(answer.answer.hash);
            break;
        case "conversion_result":
            const a = answer.answer;
            var txt = a.unit_from_number + " " + a.unit_from_name + " = " + a.unit_to_number + " " + a.unit_to_name
            node = document.createTextNode(txt);
            break;
        case "wiki":
            node = document.createElement('ul');
            results = answer.answer;
            for (let i = 0; i < results.length; i++) {
                var itemNode = document.createElement('li');
                var linkNode = document.createElement('a');
                var linkAttr = document.createAttribute('href');
                linkAttr.value = results[i].url;
                linkNode.setAttributeNode(linkAttr);
                linkNode.appendChild(document.createTextNode(results[i].title));
                var descNode = document.createElement('span');
                descNode.innerHTML = results[i].snippet;
                itemNode.appendChild(linkNode);
                itemNode.appendChild(descNode);
                node.appendChild(itemNode);
            }
            break;
        default:
            node = document.createTextNode("");
    };
    return node;
}

function removeAllChildren(node) {
    while (node.firstChild) {
        node.removeChild(node.lastChild)
    }
}

window.onload = (_event) => {
    init();
};
