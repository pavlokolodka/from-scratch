class MyEventEmitter {
  /**@type {Map<string, Array<{Function} & { listener: () => void }>} */
  #eventListeners;
  /**@type {number} */
  #maxListeners;

  constructor() {
    this.#eventListeners = new Map();
    this.#maxListeners = 10;
  }

  /**
   * Registers a listener to be called whenever the event is emitted.
   *
   * @param {string | symbol} event - The name of the event.
   * @param {Function} listener - The callback function.
   * @returns {this}
   */
  on(event, listener) {
    this.#validateListener(listener);

    const listeners = this.#eventListeners.get(String(event)) || [];

    if (listeners.length > this.#maxListeners) {
      this.#maxListenersWarn(event, listeners.length);
    }

    this.#eventListeners.set(String(event), [...listeners, listener]);

    return this;
  }

  /**
   * Registers a one-time listener for the event. It will be removed after the first call.
   *
   * @param {string | symbol} event - The name of the event.
   * @param {Function} listener - The callback function.
   * @returns {this}
   */
  once(event, listener) {
    this.#validateListener(listener);

    const cb = (...args) => {
      this.removeListener(event, cb);
      listener(...args);
    };
    cb.listener = listener;

    return this.on(event, cb);
  }

  /**
   * Adds a listener to the beginning of the listeners array for the specified event.
   *
   * @param {string | symbol} event - The name of the event.
   * @param {Function} listener - The callback function.
   * @returns {this}
   */
  prependListener(event, listener) {
    this.#validateListener(listener);
    const listeners = this.#eventListeners.get(String(event)) || [];

    if (listeners.length > this.#maxListeners) {
      this.#maxListenersWarn(event, listeners.length);
    }

    this.#eventListeners.set(String(event), [listener, ...listeners]);

    return this;
  }

  /**
   * Adds a one-time listener to the beginning of the listeners array for the specified event.
   *
   * @param {string | symbol} event - The name of the event.
   * @param {Function} listener - The callback function.
   * @returns {this}
   */
  prependOnceListener(event, listener) {
    this.#validateListener(listener);

    const cb = (...args) => {
      this.removeListener(event, cb);
      listener(...args);
    };
    cb.listener = listener;

    return this.prependListener(event, cb);
  }

  /**
   * Emits an event, invoking all registered listeners with the provided arguments.
   *
   * @param {string | symbol} event - The name of the event.
   * @param {...any} values - Arguments passed to each listener.
   * @returns {this}
   */
  emit(event, ...values) {
    const listeners = this.#eventListeners.get(String(event)) || [];

    listeners.forEach((listener) => listener(...values));

    return this;
  }

  /**
   * Removes a specific listener from the event.
   *
   * @param {string | symbol} event - The name of the event.
   * @param {Function} listener - The callback function to remove.
   * @returns {this}
   */
  removeListener(event, listener) {
    this.#validateListener(listener);

    const listeners = this.#eventListeners.get(String(event)) || [];

    const index = listeners.findIndex(
      (el) => el === listener || el.listener === listener
    );

    if (index !== -1) {
      const updatedListeners = listeners.toSpliced(index, 1);
      updatedListeners.length
        ? this.#eventListeners.set(String(event), updatedListeners)
        : this.#eventListeners.delete(event);
    }

    return this;
  }

  /**
   * Removes all listeners for a specific event, or all events if no event is specified.
   *
   * @param {string | symbol} [event] - The name of the event (optional).
   * @returns {this}
   */
  removeAllListeners(event) {
    if (event === undefined) {
      this.#eventListeners = new Map();
    }

    this.#eventListeners.delete(String(event));

    return this;
  }

  /**
   * Returns the number of listeners for a given event.
   *
   * @param {string | symbol} event - The name of the event.
   * @param {Function} [listener] - If provided, counts only this specific listener.
   * @returns {number}
   */
  listenerCount(event, listener) {
    if (listener === undefined) {
      return (this.#eventListeners.get(String(event)) || []).length;
    }

    this.#validateListener(listener);

    const listeners = this.#eventListeners.get(String(event));

    if (!listeners) return 0;

    return listeners.filter((l) => l === listener).length;
  }

  /**
   * Returns an array of all event names that have listeners.
   *
   * @returns {Array<string | symbol>}
   */
  eventNames() {
    return [...this.#eventListeners.keys()];
  }

  /**
   * Returns an array of listener functions for the given event.
   *
   * @param {string | symbol} event - The name of the event.
   * @returns {Function[]}
   */
  listeners(event) {
    const listeners = this.#eventListeners.get(event) || [];

    return listeners.map((el) =>
      typeof el.listener === "function" ? el.listener : el
    );
  }

  /**
   * Returns an array of raw listener functions for the given event.
   * For `.once()` listeners, this returns the internal wrapper function.
   *
   * @param {string | symbol} event - The name of the event.
   * @returns {Function[]}
   */
  rawListeners(event) {
    return [...(this.#eventListeners.get(event) || [])];
  }

  /**
   * Returns the current maximum number of listeners for any single event.
   *
   * @returns {number}
   */
  getMaxListeners() {
    return this.#maxListeners;
  }

  /**
   * Sets the maximum number of listeners for any single event.
   *
   * @param {number} number - The new maximum number.
   */
  setMaxListeners(number) {
    if (typeof number !== "number") {
      throw new Error(
        `The "setMaxListeners" argument must be of type number. Received ${String(
          number
        )}`
      );
    }

    this.#maxListeners = number;
  }

  #validateListener(cb) {
    if (typeof cb !== "function") {
      throw new Error(`Listener should be a function, received ${String(cb)}`);
    }
  }

  #maxListenersWarn(event, listenerCount) {
    console.warn(
      `MaxListenersExceededWarning: Possible ${
        this.constructor.name
      } memory leak detected. ${listenerCount} ${event} listeners added to [${
        this.constructor.name
      }]. MaxListeners is ${
        this.#maxListeners
      }. Use emitter.setMaxListeners() to increase limit`
    );
  }

  //
  // Aliases
  //

  off(event, listener) {
    return this.removeListener(event, listener);
  }

  addListener(event, listener) {
    return this.on(event, listener);
  }
}

module.exports = MyEventEmitter;
