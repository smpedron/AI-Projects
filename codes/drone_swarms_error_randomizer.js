Qualtrics.SurveyEngine.addOnload(function()
{
  /*Place your JavaScript here to run when the page loads*/
    function getRandomDroneNumber() {
      var numbers = [5, 15, 35];
      var randomIndex = Math.floor(Math.random() * numbers.length);
      return numbers[randomIndex];
    }
  
  var n_swarms = getRandomDroneNumber();
  
  Qualtrics.SurveyEngine.setEmbeddedData('n_swarms', n_swarms);
  
  function getRandomErrorRate() {
    var numbers = [0.01, 0.05, 0.1, 0.2];
    var randomIndex = Math.floor(Math.random() * numbers.length);
    return numbers[randomIndex];
  }
  
  var p_error = getRandomErrorRate();
  
  Qualtrics.SurveyEngine.setEmbeddedData('p_error', p_error);
  
  var human_accuracy = 0.9;
  var drone_accuracy = 0.8;
  
  Qualtrics.SurveyEngine.setEmbeddedData('human_accuracy', human_accuracy);
  Qualtrics.SurveyEngine.setEmbeddedData('drone_accuracy', drone_accuracy);
  
  function getBinomialRandom(n, p) {
    if (n < 0 || !Number.isInteger(n) || p < 0 || p > 1) {
      throw new Error("Invalid parameters: n must be a non-negative integer, p must be between 0 and 1.");
    }
    
    let successes = 0;
    for (let i = 0; i < n; i++) {
      if (Math.random() < p) {
        successes++;
      }
    }
    return successes;
  }
  
  function drawBinomialArray(arrayLength, n, p) {
    var result = [];
    for (let i = 0; i < arrayLength; i++) {
      result.push(getBinomialRandom(n, p));
    }
    return result;
  }
  
  var trial1 = drawBinomialArray(35, 1, p_error);
  var trial2 = drawBinomialArray(35, 1, p_error);
  var trial3 = drawBinomialArray(35, 1, p_error);
  var trial4 = drawBinomialArray(35, 1, p_error);
  var trial5 = drawBinomialArray(35, 1, p_error);
  var trial6 = drawBinomialArray(35, 1, p_error);
  
  Qualtrics.SurveyEngine.setEmbeddedData('trial1', trial1);
  Qualtrics.SurveyEngine.setEmbeddedData('trial2', trial2);
  Qualtrics.SurveyEngine.setEmbeddedData('trial3', trial3);
  Qualtrics.SurveyEngine.setEmbeddedData('trial4', trial4);
  Qualtrics.SurveyEngine.setEmbeddedData('trial5', trial5);
  Qualtrics.SurveyEngine.setEmbeddedData('trial6', trial6);
  
  function errorArray(prevArray, n, p) {
    var result = [];
    for (let i = 0; i < prevArray.length; i++) {
      if (prevArray[i] == 0) {
        result.push(prevArray[i]);
      } else {
        result.push(getBinomialRandom(n, p));
      }
    }
    return result;
  }
  
  var humanReport1 = errorArray(trial1, 1, human_accuracy);
  var humanReport2 = errorArray(trial2, 1, human_accuracy);
  var humanReport3 = errorArray(trial3, 1, human_accuracy);
  var humanReport4 = errorArray(trial4, 1, human_accuracy);
  var humanReport5 = errorArray(trial5, 1, human_accuracy);
  var humanReport6 = errorArray(trial6, 1, human_accuracy);
  
  var droneReport1 = errorArray(trial1, 1, drone_accuracy);
  var droneReport2 = errorArray(trial2, 1, drone_accuracy);
  var droneReport3 = errorArray(trial3, 1, drone_accuracy);
  var droneReport4 = errorArray(trial4, 1, drone_accuracy);
  var droneReport5 = errorArray(trial5, 1, drone_accuracy);
  var droneReport6 = errorArray(trial6, 1, drone_accuracy);
  
  Qualtrics.SurveyEngine.setEmbeddedData('humanReport1', humanReport1);
  Qualtrics.SurveyEngine.setEmbeddedData('humanReport2', humanReport2);
  Qualtrics.SurveyEngine.setEmbeddedData('humanReport3', humanReport3);
  Qualtrics.SurveyEngine.setEmbeddedData('humanReport4', humanReport4);
  Qualtrics.SurveyEngine.setEmbeddedData('humanReport5', humanReport5);
  Qualtrics.SurveyEngine.setEmbeddedData('humanReport6', humanReport6);
  
  Qualtrics.SurveyEngine.setEmbeddedData('droneReport1', droneReport1);
  Qualtrics.SurveyEngine.setEmbeddedData('droneReport2', droneReport2);
  Qualtrics.SurveyEngine.setEmbeddedData('droneReport3', droneReport3);
  Qualtrics.SurveyEngine.setEmbeddedData('droneReport4', droneReport4);
  Qualtrics.SurveyEngine.setEmbeddedData('droneReport5', droneReport5);
  Qualtrics.SurveyEngine.setEmbeddedData('droneReport6', droneReport6);
  
  function labelErrors(prevArray) {
    var result = [];
    for (let i = 0; i < prevArray.length; i++) {
      if (prevArray[i] === 0) {
        result.push('<span style="color: green;">NORMAL</span>');
      } else {
        result.push('<span style="color: red; font-weight: bold;">ERROR</span>');
      }
    }
    return result;
  }
  
  var humanText1 = labelErrors(humanReport1);
  var humanText2 = labelErrors(humanReport2);
  var humanText3 = labelErrors(humanReport3);
  var humanText4 = labelErrors(humanReport4);
  var humanText5 = labelErrors(humanReport5);
  var humanText6 = labelErrors(humanReport6);
  
  var droneText1 = labelErrors(droneReport1);
  var droneText2 = labelErrors(droneReport2);
  var droneText3 = labelErrors(droneReport3);
  var droneText4 = labelErrors(droneReport4);
  var droneText5 = labelErrors(droneReport5);
  var droneText6 = labelErrors(droneReport6);
  
  Qualtrics.SurveyEngine.setEmbeddedData('humanText1_1', humanText1[0]);
  Qualtrics.SurveyEngine.setEmbeddedData('humanText1_2', humanText1[1]);
  Qualtrics.SurveyEngine.setEmbeddedData('humanText1_3', humanText1[2]);
  Qualtrics.SurveyEngine.setEmbeddedData('humanText1_4', humanText1[3]);
  Qualtrics.SurveyEngine.setEmbeddedData('humanText1_5', humanText1[4]);
  Qualtrics.SurveyEngine.setEmbeddedData('humanText1_6', humanText1[5]);
  Qualtrics.SurveyEngine.setEmbeddedData('humanText1_7', humanText1[6]);
  Qualtrics.SurveyEngine.setEmbeddedData('humanText1_8', humanText1[7]);
  Qualtrics.SurveyEngine.setEmbeddedData('humanText1_9', humanText1[8]);
  Qualtrics.SurveyEngine.setEmbeddedData('humanText1_10', humanText1[9]);
  Qualtrics.SurveyEngine.setEmbeddedData('humanText1_11', humanText1[10]);
  Qualtrics.SurveyEngine.setEmbeddedData('humanText1_12', humanText1[11]);
  Qualtrics.SurveyEngine.setEmbeddedData('humanText1_13', humanText1[12]);
  Qualtrics.SurveyEngine.setEmbeddedData('humanText1_14', humanText1[13]);
  Qualtrics.SurveyEngine.setEmbeddedData('humanText1_15', humanText1[14]);
  Qualtrics.SurveyEngine.setEmbeddedData('humanText1_16', humanText1[15]);
  Qualtrics.SurveyEngine.setEmbeddedData('humanText1_17', humanText1[16]);
  Qualtrics.SurveyEngine.setEmbeddedData('humanText1_18', humanText1[17]);
  Qualtrics.SurveyEngine.setEmbeddedData('humanText1_19', humanText1[18]);
  Qualtrics.SurveyEngine.setEmbeddedData('humanText1_20', humanText1[19]);
  Qualtrics.SurveyEngine.setEmbeddedData('humanText1_21', humanText1[20]);
  Qualtrics.SurveyEngine.setEmbeddedData('humanText1_22', humanText1[21]);
  Qualtrics.SurveyEngine.setEmbeddedData('humanText1_23', humanText1[22]);
  Qualtrics.SurveyEngine.setEmbeddedData('humanText1_24', humanText1[23]);
  Qualtrics.SurveyEngine.setEmbeddedData('humanText1_25', humanText1[24]);
  Qualtrics.SurveyEngine.setEmbeddedData('humanText1_26', humanText1[25]);
  Qualtrics.SurveyEngine.setEmbeddedData('humanText1_27', humanText1[26]);
  Qualtrics.SurveyEngine.setEmbeddedData('humanText1_28', humanText1[27]);
  Qualtrics.SurveyEngine.setEmbeddedData('humanText1_29', humanText1[28]);
  Qualtrics.SurveyEngine.setEmbeddedData('humanText1_30', humanText1[29]);
  Qualtrics.SurveyEngine.setEmbeddedData('humanText1_31', humanText1[30]);
  Qualtrics.SurveyEngine.setEmbeddedData('humanText1_32', humanText1[31]);
  Qualtrics.SurveyEngine.setEmbeddedData('humanText1_33', humanText1[32]);
  Qualtrics.SurveyEngine.setEmbeddedData('humanText1_34', humanText1[33]);
  Qualtrics.SurveyEngine.setEmbeddedData('humanText1_35', humanText1[34]);
  
  Qualtrics.SurveyEngine.setEmbeddedData('droneText1_1', droneText1[0]);
  Qualtrics.SurveyEngine.setEmbeddedData('droneText1_2', droneText1[1]);
  Qualtrics.SurveyEngine.setEmbeddedData('droneText1_3', droneText1[2]);
  Qualtrics.SurveyEngine.setEmbeddedData('droneText1_4', droneText1[3]);
  Qualtrics.SurveyEngine.setEmbeddedData('droneText1_5', droneText1[4]);
  Qualtrics.SurveyEngine.setEmbeddedData('droneText1_6', droneText1[5]);
  Qualtrics.SurveyEngine.setEmbeddedData('droneText1_7', droneText1[6]);
  Qualtrics.SurveyEngine.setEmbeddedData('droneText1_8', droneText1[7]);
  Qualtrics.SurveyEngine.setEmbeddedData('droneText1_9', droneText1[8]);
  Qualtrics.SurveyEngine.setEmbeddedData('droneText1_10', droneText1[9]);
  Qualtrics.SurveyEngine.setEmbeddedData('droneText1_11', droneText1[10]);
  Qualtrics.SurveyEngine.setEmbeddedData('droneText1_12', droneText1[11]);
  Qualtrics.SurveyEngine.setEmbeddedData('droneText1_13', droneText1[12]);
  Qualtrics.SurveyEngine.setEmbeddedData('droneText1_14', droneText1[13]);
  Qualtrics.SurveyEngine.setEmbeddedData('droneText1_15', droneText1[14]);
  Qualtrics.SurveyEngine.setEmbeddedData('droneText1_16', droneText1[15]);
  Qualtrics.SurveyEngine.setEmbeddedData('droneText1_17', droneText1[16]);
  Qualtrics.SurveyEngine.setEmbeddedData('droneText1_18', droneText1[17]);
  Qualtrics.SurveyEngine.setEmbeddedData('droneText1_19', droneText1[18]);
  Qualtrics.SurveyEngine.setEmbeddedData('droneText1_20', droneText1[19]);
  Qualtrics.SurveyEngine.setEmbeddedData('droneText1_21', droneText1[20]);
  Qualtrics.SurveyEngine.setEmbeddedData('droneText1_22', droneText1[21]);
  Qualtrics.SurveyEngine.setEmbeddedData('droneText1_23', droneText1[22]);
  Qualtrics.SurveyEngine.setEmbeddedData('droneText1_24', droneText1[23]);
  Qualtrics.SurveyEngine.setEmbeddedData('droneText1_25', droneText1[24]);
  Qualtrics.SurveyEngine.setEmbeddedData('droneText1_26', droneText1[25]);
  Qualtrics.SurveyEngine.setEmbeddedData('droneText1_27', droneText1[26]);
  Qualtrics.SurveyEngine.setEmbeddedData('droneText1_28', droneText1[27]);
  Qualtrics.SurveyEngine.setEmbeddedData('droneText1_29', droneText1[28]);
  Qualtrics.SurveyEngine.setEmbeddedData('droneText1_30', droneText1[29]);
  Qualtrics.SurveyEngine.setEmbeddedData('droneText1_31', droneText1[30]);
  Qualtrics.SurveyEngine.setEmbeddedData('droneText1_32', droneText1[31]);
  Qualtrics.SurveyEngine.setEmbeddedData('droneText1_33', droneText1[32]);
  Qualtrics.SurveyEngine.setEmbeddedData('droneText1_34', droneText1[33]);
  Qualtrics.SurveyEngine.setEmbeddedData('droneText1_35', droneText1[34]);
  
});
