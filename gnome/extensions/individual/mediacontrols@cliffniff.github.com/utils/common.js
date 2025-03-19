const DEBUG = false;

export const enumValueByIndex = (enumObject, index) => {
    return Object.values(enumObject)[index];
};

export const enumKeyByValue = (enumObject, value) => {
    return Object.keys(enumObject).find((key) => enumObject[key] === value);
};

export const debugLog = (...args) => {
    if (DEBUG) {
        console.log("[Media Controls]", ...args);
    }
};

export const errorLog = (...args) => {
    console.error("[Media Controls]", "Error:", ...args);
};

export const handleError = (error) => {
    errorLog(error);
    return null;
};

export const msToHHMMSS = (ms) => {
    const seconds = Math.floor(ms / 1000);
    const minutes = Math.floor(ms / 60000);
    const hours = Math.floor(ms / 3600000);
    const secondsString = String(seconds % 60).padStart(2, "0");
    const minutesString = String(minutes % 60).padStart(2, "0");
    const hoursString = String(hours).padStart(2, "0");

    if (hours > 0) {
        return `${hoursString}:${minutesString}:${secondsString}`;
    }
    else {
        return `${minutesString}:${secondsString}`;
    }
};
