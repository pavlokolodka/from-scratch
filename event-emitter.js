class MyEventEmitter {
  /**@type {Map<string, Array<{Function}>} */
  #eventListeners;

  constructor() {
    this.#eventListeners = new Map();
  }

  on(event, listener) {
    this.#validateListener(listener);

    const listeners = this.#eventListeners.get(String(event)) || [];

    this.#eventListeners.set(String(event), [...listeners, listener]);

    return this;
  }

  once(event, listener) {
    this.#validateListener(listener);

    const cb = (...args) => {
      this.removeListener(event, cb);
      listener(...args);
    };

    return this.on(event, cb);
  }

  emit(event, ...values) {
    const listeners = this.#eventListeners.get(String(event)) || [];

    listeners.forEach((listener) => listener(...values));

    return this;
  }

  removeListener(event, listener) {
    this.#validateListener(listener);

    const listeners = this.#eventListeners.get(String(event)) || [];

    const index = listeners.indexOf(listener);

    if (index !== -1) {
      const updatedListeners = listeners.toSpliced(index, 1);
      updatedListeners.length
        ? this.#eventListeners.set(String(event), updatedListeners)
        : this.#eventListeners.delete(event);
    }

    return this;
  }

  removeAllListeners(event) {
    if (event === undefined) {
      this.#eventListeners = new Map();
    }

    this.#eventListeners.delete(String(event));

    return this;
  }

  listenerCount(event, listener) {
    if (listener === undefined) {
      return (this.#eventListeners.get(String(event)) || []).length;
    }

    this.#validateListener(listener);

    const listeners = this.#eventListeners.get(String(event));

    if (!listeners) return 0;

    return listeners.filter((l) => l === listener).length;
  }

  eventNames() {
    return [...this.#eventListeners.keys()];
  }

  #validateListener(cb) {
    if (typeof cb !== "function") {
      throw new Error(`Listener should be a function, received ${cb}`);
    }
  }

  /**
   * Aliases
   */

  off(event, listener) {
    return this.removeListener(event, listener);
  }

  addListener(event, listener) {
    return this.on(event, listener);
  }
}

module.exports = MyEventEmitter;
