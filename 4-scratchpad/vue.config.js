module.exports = {
  configureWebpack: {
    module: {
      rules: [
        {
          test: /\.lisp$/i,
          use: 'raw-loader'
        }
      ]
    }
  },
  chainWebpack: config => {
    // config.module.rules.delete('raw');

    /*config.module
      .rule('lisp')
      .test(() => true)
      //.test(/^\.lisp$/i)
      .use('file-loader')
        .loader('file-loader')
        .end()*/

    config.module
      .rule('lisp')
      .test(/\.lisp$/)
      .use('raw-loader')
      .loader('raw-loader')
      .end()
  }
};
