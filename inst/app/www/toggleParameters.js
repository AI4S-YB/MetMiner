function toggleParameters(elementId) {
  var div = document.getElementById(elementId);
  var right = div.style.right;
  if (right === '-300px') {
    div.style.right = '0px';
    div.style.transition = 'right 0.8s ease-in-out';
    return;
  } else if (right === '0px') {
    div.style.right = '-300px';
    div.style.transition = 'right 0.8s ease-in-out';
    return;
  }
}
